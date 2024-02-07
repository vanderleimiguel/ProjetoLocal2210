#INCLUDE 'PROTHEUS.CH'
#INCLUDE 'RWMAKE.CH'
#INCLUDE 'FONT.CH'
#INCLUDE 'COLORS.CH'
#include "TOTVS.CH"
#INCLUDE "TBICONN.CH"
#include "TOPCONN.CH"
#Include 'FWMVCDef.ch'
#INCLUDE "FWPrintSetup.ch"
#INCLUDE 'PARMTYPE.CH'
#INCLUDE "RPTDEF.CH"

/*/{Protheus.doc} XAG0109C.PRW
Programa Executor automatico do Faturamento 
@author Geovani S Mauricio
@since Dez/2023
@version 1.0
@see (links_or_references)

Parametros
MV_XROMAN  : identifica se executa ou n„o a rotina de Romaneio
MV_XBOLETO : identifica se executa ou n„o a rotina de Boleto
MV_XSRAUTO : identifica a serie a ser utilizada na geraÁ„o da nota fiscal.
MV_XNFEIMP : identifica se ir· gerar, transmitir e imprimir diretamente a DANFE

/*/
User FUNCTION XAG0109C(_cPedido, _cSeqPed) //U_XAG0109C('A04721')

	Default _cSeqPed := ""
	Local _aArea     := {}

	Private _lRet 	  := .f.
	Private _cSerie   := SuperGetMV( "MV_XSRAUTO" , .F. , "2" )  //Seria padrao para nota fiscal
	Private _cDoc     := ""
	Private _cPrinter := SuperGetMV( "MV_XPRAUTO" , .F. , "Microsoft Print to PDF" )  //Impressora para nota e boleto

	if Empty(_cSerie)
		MSGALERT( 'SERIE NOTA FISCAL NAO DEFINIDA NO PARAMETRO MV_XSRAUTO. VERIFIQUE !!!', "ERRO" )
		RETURN
	endif
	//-------------------------------------------
	// Chama rotina para geraÁ„o da nota fiscal
	//-------------------------------------------
	Processa( {|_lRet| u_109CNota(_cPedido, _cSeqPed) }, "Aguarde...", "Realizando GeraÁ„o da Nota Fiscal...",.T.)
	if _lRet
		//chama rotina de Impress„o de Etiqueta
		u_XAG0125(_cDoc,_cSerie)
	Endif

	If _lRet
		// Executa Transmiss„o da nota fiscal
		Processa( {|_lRet| u_109CTrans(_cDoc, _cSerie) }, "Aguarde...", "Realizando Transmiss„o da Nota Fiscal...",.T.)
	endif

	If _lRet
		//Processa( {|_lRet| u_109IDANFE(_cDoc, _cSerie) }, "Aguarde...", "Realizando Impress„o da Nota Fiscal...",.T.)
	Endif

	If _lRet
		Processa( {|| u_109BOLETO(_cDoc, _cSerie) }, "Aguarde...", "Realizando GeraÁ„o e Impressao Boleto...",.T.)//u_109BOLETO('000003261', '2')
	Endif

	If _lRet
		Processa( {|| u_109ROMANEIO(_cDoc, _cSerie) }, "Aguarde...", "Realizando Impress„o do Romaneio...",.T.)//u_109ROMANEIO('000003261', '2')
	Endif


	Restarea(_aArea)

Return

//-----------------------------------
// Gerar nota fiscal
//-----------------------------------
User Function 109CNota( _cPedido,_cSeqPed)

	Local aPvlDocS := {}
	Local nPrcVen := 0
	Local cEmbExp := ""
	Local aNotas  := {}

	SC5->(DbSetOrder(1))
	SC5->(MsSeek(xFilial("SC5")+_cPedido))

	SC6->(dbSetOrder(1))
	SC6->(MsSeek(xFilial("SC6")+SC5->C5_NUM))

	//ù necessùrio carregar o grupo de perguntas MT460A, se nùo serù executado com os valores default.
	Pergunte("MT460A",.F.)

	// Condicao de pagamento do pedido
	SE4->(DbSetOrder(1))
	SE4->(MsSeek(xFilial("SE4")+SC5->C5_CONDPAG) )  //FILIAL+CONDICAO PAGTO

	// Obter os dados de cada item do pedido de vendas liberado para gerar o Documento de Saùda
	SC6->(MsSeek(xFilial("SC6")+SC5->C5_NUM))
	While SC6->(!Eof() .And. C6_FILIAL == xFilial("SC6")) .And. SC6->C6_NUM == SC5->C5_NUM

		SC9->(DbSetOrder(1))
		SC9->(MsSeek(xFilial("SC9")+SC6->(C6_NUM+C6_ITEM))) //FILIAL+NUMERO+ITEM

		SB1->(DbSetOrder(1))
		SB1->(MsSeek(xFilial("SB1")+SC6->C6_PRODUTO))    //FILIAL+PRODUTO

		SB2->(DbSetOrder(1))
		SB2->(MsSeek(xFilial("SB2")+SC6->(C6_PRODUTO+C6_LOCAL))) //FILIAL+PRODUTO+LOCAL

		SF4->(DbSetOrder(1))
		SF4->(MsSeek(xFilial("SF4")+SC6->C6_TES))   //FILIAL+TES

		nPrcVen := SC9->C9_PRCVEN
		If ( SC5->C5_MOEDA <> 1 )
			nPrcVen := xMoeda(nPrcVen,SC5->C5_MOEDA,1,dDataBase)
		EndIf

		If AllTrim(SC9->C9_BLEST) == "" .And. AllTrim(SC9->C9_BLCRED) == "" //.AND. !Empty(SC9->C9_XDTCONF) .AND. !(SC9->C9_XSTSSEP $'G/H')
			If alltrim(_cSeqPed) == '' .OR. SC9->C9_XSREDI   == _cSeqPed
				AAdd(aPvlDocS,{ SC9->C9_PEDIDO,;
					SC9->C9_ITEM,;
					SC9->C9_SEQUEN,;
					SC9->C9_QTDLIB,;
					nPrcVen,;
					SC9->C9_PRODUTO,;
					.F.,;
					SC9->(RecNo()),;
					SC5->(RecNo()),;
					SC6->(RecNo()),;
					SE4->(RecNo()),;
					SB1->(RecNo()),;
					SB2->(RecNo()),;
					SF4->(RecNo())})
			Endif

		EndIf

		SC6->(DbSkip())
	EndDo

	Begin Transaction

		SetFunName("MATA461")

		_cDoc := MaPvlNfs(  /*aPvlNfs*/         aPvlDocS,;  // 01 - Array com os itens a serem gerados
            	           /*cSerieNFS*/       _cSerie,;    // 02 - Serie da Nota Fiscal
   		                   /*lMostraCtb*/      .F.,;       // 03 - Mostra Lanùamento Contùbil
           		           /*lAglutCtb*/       .F.,;       // 04 - Aglutina Lanùamento Contùbil
                   		   /*lCtbOnLine*/      .F.,;       // 05 - Contabiliza On-Line
	                       /*lCtbCusto*/       .T.,;       // 06 - Contabiliza Custo On-Line
	                       /*lReajuste*/       .F.,;       // 07 - Reajuste de preùo na Nota Fiscal
       		               /*nCalAcrs*/        0,;         // 08 - Tipo de Acrùscimo Financeiro
	                       /*nArredPrcLis*/    0,;         // 09 - Tipo de Arredondamento
	                       /*lAtuSA7*/         .T.,;       // 10 - Atualiza Amarraùùo Cliente x Produto
	                       /*lECF*/            .F.,;       // 11 - Cupom Fiscal
	                       /*cEmbExp*/         cEmbExp,;   // 12 - Nùmero do Embarque de Exportaùùo
       		               /*bAtuFin*/         {||},;      // 13 - Bloco de Cùdigo para complemento de atualizaùùo dos tùtulos financeiros
	                       /*bAtuPGerNF*/      {||},;      // 14 - Bloco de Cùdigo para complemento de atualizaùùo dos dados apùs a geraùùo da Nota Fiscal
	                       /*bAtuPvl*/         {||},;      // 15 - Bloco de Cùdigo de atualizaùùo do Pedido de Venda antes da geraùùo da Nota Fiscal
	                       /*bFatSE1*/         {|| .T. },; // 16 - Bloco de Cùdigo para indicar se o valor do Titulo a Receber serù gravado no campo F2_VALFAT quando o parùmetro MV_TMSMFAT estiver com o valor igual a "2".
	                       /*dDataMoe*/        dDatabase,; // 17 - Data da cotaùùo para conversùo dos valores da Moeda do Pedido de Venda para a Moeda Forte
       		               /*lJunta*/          .F.)        // 18 - Aglutina Pedido Iguais

		If !Empty(_cDoc)
			aAdd(aNotas,{_cSerie,_cDoc})
		EndIf

	End Transaction

	if Len(aNotas) > 0
		MsgAlert("Documento de Saida: " + _cSerie + "-" + _cDoc + ", gerado com sucesso!!!","AVISO")
		_lRet := .t.
	else
		MsgAlert('Nota Fiscal NAO GERADA. Verifique !!!', 'ATENCAO')
		_lRet := .f.
	endif
RETURN(_lRet)

//-----------------------------------
// Transmitir nota fiscal
//-----------------------------------
User Function 109CTrans(_cDoc, _cSerie )

	SF2->(dbSetOrder(1))//->F2_FILIAL+F2_DOC+F2_SERIE+F2_CLIENTE+F2_LOJA+F2_FORMUL+F2_TIPO
	If SF2->(dbSeek(xFilial("SF2") + _cDoc + _cSerie ))
		if Empty(sf2->f2_chvnfe)
			Processa({|lEnd| u_109GDANFE() }, "Aguarde...","Preparando dados para transmissao da nota.",.F.)
			if !Empty(sf2->f2_chvnfe)
				//MsgAlert('Nota Fiscal transmitida com sucesso !!!','AVISO')
				_lRet := .t.
			else
				MsgAlert('Transmissao da Nota Fiscal com ERRO. Favor verificar o erro no Monitor !!!','ATENCAO')
				_lRet := .f.
			endif
		endif
	Endif

Return (_lRet)

//-----------------------------------
// Transmissùo DANFE
//-----------------------------------
User Function 109GDANFE()

	Local _aArea		:= GetArea()
	Local cANfeAmbi	:= GetNewPar("MV_ANFEAMB",'2') //(1=producao,2=Homologacao)
	Local _nSlpANFe	:= GetNewPar("MV_ANFESLP",2000) //->Sleep envio Auto NF-e
	Default _lAuto	:= .F.
	Default _lImp	:= .F.
	Default _lLog	:= .F.

	_cDoc  := SF2->F2_DOC
	_cSerie:= SF2->F2_SERIE
	_nTipo := 2    // 2- Saida
	_lEnvia:= .t.
	_cCliente := SF2->F2_CLIENTE
	_cLojam   := SF2->F2_LOJA
	_lAuto    := .f.
	_lImp     := .f.
	_lLog     := .f.
	_cLog     := ''

	SF2->(dbSetOrder(1))//->F2_FILIAL+F2_DOC+F2_SERIE+F2_CLIENTE+F2_LOJA+F2_FORMUL+F2_TIPO
	SF2->(dbSeek(xFilial("SF2") + _cDoc + _cSerie + _cCliente + _cLojam))
	If _lEnvia //-> Envia NF-e
		U_109CTAutoNfe(cEmpAnt,xFilial("SF2"),"0",cANfeAmbi,SF2->F2_SERIE,SF2->F2_DOC,SF2->F2_DOC)
		Sleep(_nSlpANFe)
	Endif
	U_CTNFeMnt(_cSerie, _cDoc, _cDoc) //-> monitora a nfe

	RestArea(_aArea)
Return(_lRet)

/*/{Protheus.doc} 109CTAutoNfe
//Transmite NF-e
/*/
User Function 109CTAutoNfe(cEmpresa,cFilProc,cWait,cOpc,cSerie,cNotaIni,cNotaFim)
	Local aArea       := GetArea()
	Local aPerg       := {}
	Local lEnd        := .F.
	Local aParam      := {Space(Len(SF2->F2_SERIE)),Space(Len(SF2->F2_DOC)),Space(Len(SF2->F2_DOC))}
	Local aXML        := {}
	Local cRetorno    := ""
	Local cIdEnt      := ""
	Local cModalidade := ""
	Local cAmbiente   := ""
	Local cVersao     := ""
	Local cVersaoCTe  := ""
	Local cVersaoDpec := ""
	Local cMonitorSEF := ""
	Local cSugestao   := ""
	Local cURL        := PadR(GetNewPar("MV_SPEDURL","http://"),250)
	Local nX          := 0
	Local lOk         := .T.
	Local oWs
	Local cParNfeRem  := SM0->M0_CODIGO+SM0->M0_CODFIL+"AUTONFEREM"

	If cSerie == Nil
		MV_PAR01 := aParam[01] := PadR(ParamLoad(cParNfeRem,aPerg,1,aParam[01]),Len(SF2->F2_SERIE))
		MV_PAR02 := aParam[02] := PadR(ParamLoad(cParNfeRem,aPerg,2,aParam[02]),Len(SF2->F2_DOC))
		MV_PAR03 := aParam[03] := PadR(ParamLoad(cParNfeRem,aPerg,3,aParam[03]),Len(SF2->F2_DOC))
	Else
		MV_PAR01 := aParam[01] := cSerie
		MV_PAR02 := aParam[02] := cNotaIni
		MV_PAR03 := aParam[03] := cNotaFim
	EndIf

	If .T.//CTIsRdy()

//-> Obtem o codigo da entidade

		cIdEnt := u_109GetIdEnt()

		If !Empty(cIdEnt)

//-> Obtem o ambiente de execucao do Totvs Services SPED
			oWS := WsSpedCfgNFe():New()
			oWS:cUSERTOKEN := "TOTVS"
			oWS:cID_ENT    := cIdEnt
			oWS:nAmbiente  := 0
			oWS:_URL       := AllTrim(cURL)+"/SPEDCFGNFe.apw"
			lOk			   := execWSRet( oWS, "CFGAMBIENTE")
			If lOk
				cAmbiente := oWS:cCfgAmbienteResult
			Else
				Conout(IIf(Empty(GetWscError(3)),GetWscError(1),GetWscError(3)))
			EndIf
//-> Obtem a modalidade de execucao do Totvs Services SPED
			If lOk
				oWS:cUSERTOKEN := "TOTVS"
				oWS:cID_ENT    := cIdEnt
				oWS:nModalidade:= 0
				oWS:_URL       := AllTrim(cURL)+"/SPEDCFGNFe.apw"
				oWs:cModelo	   := "55"
				lOk 		   := execWSRet( oWS, "CFGModalidade" )
				If lOk
					cModalidade:= oWS:cCfgModalidadeResult
				Else
					Conout(IIf(Empty(GetWscError(3)),GetWscError(1),GetWscError(3)))
				EndIf
			EndIf
//-> Obtem a versao de trabalho da NFe do Totvs Services SPED
			If lOk
				oWS:cUSERTOKEN := "TOTVS"
				oWS:cID_ENT    := cIdEnt
				oWS:cVersao    := "0.00"
				oWS:_URL       := AllTrim(cURL)+"/SPEDCFGNFe.apw"
				lOk			   := execWSRet( oWs, "CFGVersao" )
				If lOk
					cVersao    := oWS:cCfgVersaoResult
				Else
					Conout(IIf(Empty(GetWscError(3)),GetWscError(1),GetWscError(3)))
				EndIf
			EndIf
			If lOk
				oWS:cUSERTOKEN := "TOTVS"
				oWS:cID_ENT    := cIdEnt
				oWS:cVersao    := "0.00"
				oWS:_URL       := AllTrim(cURL)+"/SPEDCFGNFe.apw"
				lOk 		   := execWSRet( oWs, "CFGVersaoCTe" )
				If lOk
					cVersaoCTe := oWS:cCfgVersaoCTeResult
				Else
					Conout(IIf(Empty(GetWscError(3)),GetWscError(1),GetWscError(3)))
				EndIf
			EndIf
			If lOk
				oWS:cUSERTOKEN := "TOTVS"
				oWS:cID_ENT    := cIdEnt
				oWS:cVersao    := "0.00"
				oWS:_URL       := AllTrim(cURL)+"/SPEDCFGNFe.apw"
				lOk			   := execWSRet( oWs, "CFGVersaoDpec" )
				If lOk
					cVersaoDpec:= oWS:cCfgVersaoDpecResult
				Else
					Conout(IIf(Empty(GetWscError(3)),GetWscError(1),GetWscError(3)))
				EndIf
			EndIf
//-> Verifica Status Sefaz
			If lOk
				oWS:= WSNFeSBRA():New()
				oWS:cUSERTOKEN := "TOTVS"
				oWS:cID_ENT    := cIdEnt
				oWS:_URL       := AllTrim(cURL)+"/NFeSBRA.apw"
				lOk := oWS:MONITORSEFAZMODELO()
				If lOk
					aXML := oWS:oWsMonitorSefazModeloResult:OWSMONITORSTATUSSEFAZMODELO
					For nX := 1 To Len(aXML)
						Do Case
						Case aXML[nX]:cModelo == "55"
							cMonitorSEF += "- NFe"+CRLF
							cMonitorSEF += "Versao do layout: "+cVersao+CRLF
							If !Empty(aXML[nX]:cSugestao)
								cSugestao += 'Sugestao'+"(NFe)"+": "+aXML[nX]:cSugestao+CRLF
							EndIf

						Case aXML[nX]:cModelo == "57"
							cMonitorSEF += "- CTe"+CRLF
							cMonitorSEF += "Versao do layout: "+cVersaoCTe+CRLF
							If !Empty(aXML[nX]:cSugestao)
								cSugestao += 'Sugestao'+"(CTe)"+": "+aXML[nX]:cSugestao+CRLF
							EndIf
						EndCase
						cMonitorSEF += Space(6)+"Versao da mensagem"+": "+aXML[nX]:cVersaoMensagem+CRLF
						cMonitorSEF += Space(6)+"Codigo do Status"+": "+aXML[nX]:cStatusCodigo+"-"+aXML[nX]:cStatusMensagem+CRLF
						cMonitorSEF += Space(6)+"UF Origem"+": "+aXML[nX]:cUFOrigem
						If !Empty(aXML[nX]:cUFResposta)
							cMonitorSEF += "("+aXML[nX]:cUFResposta+")"+CRLF //"UF Resposta"
						Else
							cMonitorSEF += CRLF
						EndIf
						If aXML[nX]:nTempoMedioSEF <> Nil
							cMonitorSEF += Space(6)+"Tempo de espera"+": "+Str(aXML[nX]:nTempoMedioSEF,6)+CRLF
						EndIf
						If !Empty(aXML[nX]:cMotivo)
							cMonitorSEF += Space(6)+"Motivo"+": "+aXML[nX]:cMotivo+CRLF
						EndIf
						If !Empty(aXML[nX]:cObservacao)
							cMonitorSEF += Space(6)+'Observacao'+": "+aXML[nX]:cObservacao+CRLF
						EndIf
					Next nX
				EndIf
			EndIf

			Conout("[JOB  ]["+cIdEnt+"] - Iniciando transmissao NF-e de saida!")
			cRetorno := SpedNFeTrf("SF2",aParam[1],aParam[2],aParam[3],cIdEnt,cAmbiente,cModalidade,cVersao,@lEnd,.F.,.T.)
			Conout("[JOB  ]["+cIdEnt+"] - "+cRetorno)

		EndIf
	Else
		Conout("SPED","Execute o modulo de configuracao do servico, antes de utilizar esta opcao!!!")
	EndIf

	RestArea(aArea)
Return

/*/
	Rotina : 109GetIdEnt
	Obtem o codigo da entidade apos enviar o post para o Totvs
/*/
User Function 109GetIdEnt()

	Local aArea  := GetArea()
	Local cIdEnt := ""
	Local cURL   := PadR(GetNewPar("MV_SPEDURL","http://"),250)
	Local oWs

	oWS := WsSPEDAdm():New()
	oWS:cUSERTOKEN := "TOTVS"

	oWS:oWSEMPRESA:cCNPJ       := IIF(SM0->M0_TPINSC==2 .Or. Empty(SM0->M0_TPINSC),SM0->M0_CGC,"")
	oWS:oWSEMPRESA:cCPF        := IIF(SM0->M0_TPINSC==3,SM0->M0_CGC,"")
	oWS:oWSEMPRESA:cIE         := SM0->M0_INSC
	oWS:oWSEMPRESA:cIM         := SM0->M0_INSCM
	oWS:oWSEMPRESA:cNOME       := SM0->M0_NOMECOM
	oWS:oWSEMPRESA:cFANTASIA   := SM0->M0_NOME
	oWS:oWSEMPRESA:cENDERECO   := FisGetEnd(SM0->M0_ENDENT)[1]
	oWS:oWSEMPRESA:cNUM        := FisGetEnd(SM0->M0_ENDENT)[3]
	oWS:oWSEMPRESA:cCOMPL      := FisGetEnd(SM0->M0_ENDENT)[4]
	oWS:oWSEMPRESA:cUF         := SM0->M0_ESTENT
	oWS:oWSEMPRESA:cCEP        := SM0->M0_CEPENT
	oWS:oWSEMPRESA:cCOD_MUN    := SM0->M0_CODMUN
	oWS:oWSEMPRESA:cCOD_PAIS   := "1058"
	oWS:oWSEMPRESA:cBAIRRO     := SM0->M0_BAIRENT
	oWS:oWSEMPRESA:cMUN        := SM0->M0_CIDENT
	oWS:oWSEMPRESA:cCEP_CP     := Nil
	oWS:oWSEMPRESA:cCP         := Nil
	oWS:oWSEMPRESA:cDDD        := Str(FisGetTel(SM0->M0_TEL)[2],3)
	oWS:oWSEMPRESA:cFONE       := AllTrim(Str(FisGetTel(SM0->M0_TEL)[3],15))
	oWS:oWSEMPRESA:cFAX        := AllTrim(Str(FisGetTel(SM0->M0_FAX)[3],15))
	oWS:oWSEMPRESA:cEMAIL      := UsrRetMail(RetCodUsr())
	oWS:oWSEMPRESA:cNIRE       := SM0->M0_NIRE
	oWS:oWSEMPRESA:dDTRE       := SM0->M0_DTRE
	oWS:oWSEMPRESA:cNIT        := IIF(SM0->M0_TPINSC==1,SM0->M0_CGC,"")
	oWS:oWSEMPRESA:cINDSITESP  := ""
	oWS:oWSEMPRESA:cID_MATRIZ  := ""
	oWS:oWSOUTRASINSCRICOES:oWSInscricao := SPEDADM_ARRAYOFSPED_GENERICSTRUCT():New()
	oWS:_URL := AllTrim(cURL)+"/SPEDADM.apw"
	If oWs:ADMEMPRESAS()
		cIdEnt  := oWs:cADMEMPRESASRESULT
	Else
		Aviso("SPED",IIf(Empty(GetWscError(3)),GetWscError(1),GetWscError(3)),{'Erro'},3)
	EndIf

	RestArea(aArea)
Return(cIdEnt)

User Function 109IDANFE(_cNota,_cSerie)//u_109IDANFE('000003268','2')
	Local aArea     := GetArea()
	Local cPasta	:= ""
	Local cIdent    := ""
	Local oDanfe    := Nil
	Local lEnd      := .F.
	Local nTamNota  := TamSX3('F2_DOC')[1]
	Local nTamSerie := TamSX3('F2_SERIE')[1]
	Local _i
	Local _lRet     := .F.
	Private PixelX
	Private PixelY
	Private nConsNeg
	Private nConsTex
	Private oRetNF
	Private lPtImpBol
	Private aNotasBol
	Private nColAux

	//  Acha nota fiscal
	aStNotas := {}
	SF2->(dbsetorder(1))
	SF2->(dbSeek(xFilial('SF2') + _cNota + _cSerie))
	While SF2->(!eof()) .and. SF2->F2_FILIAL == xFilial("SF2") .and. Alltrim(sf2->f2_doc) == Alltrim(_cNota) .and. Alltrim(sf2->f2_serie) == Alltrim(_cSerie)
		If !Empty(SF2->F2_CHVNFE)
			aadd( aStNotas, {SF2->F2_SERIE, SF2->F2_DOC, SF2->F2_EMISSAO, SF2->( Recno() )} )
		Endif
		SF2->(dbSkip())
	End

	If Empty(Len(aStNotas))
		MsgAlert('Este PEDIDO nao possui DANFE transmitida. Verifique !!!', 'ATENCAO')
		_lRet := .F.
	Else
		_nTipo  := 2     		// 2= Saida
		cIdent := RetIdEnti()	// Pega o IDENT da empresa
		For _i := 1 to Len(aStNotas)
			_cSerie   	:= aStNotas[_i][1]
			_cDoc     	:= aStNotas[_i][2]
			_dEmis    	:= aStNotas[_i][3]
			SF2->(dbsetorder(1))
			SF2->(dbSeek(xFilial('SF2') + _cDoc + _cSerie))
			cPasta 		:= "C:\TEMP\"
			_lExibe   	:= .F.
			//Pega o IDENT da empresa
			cIdent 		:= RetIdEnti() //->Entidade

			//Se o ˙ltimo caracter da pasta n„o for barra, ser· barra para integridade
			If SubStr(cPasta, Len(cPasta), 1) != "\"
				cPasta += "\"
			EndIf

			//cFilePrint := "danfe_"+Alltrim(_cDoc)+".PDF"
			cFilePrint := "danfe_"+Alltrim(_cNota) + "_" + dToS(Date()) + "_" + StrTran(Time(), ":", "-")

			//Define as perguntas da DANFE
			//Pergunte("NFSIGW",.F.)
			MV_PAR01 := PadR(_cNota,  nTamNota)     //Nota Inicial
			MV_PAR02 := PadR(_cNota,  nTamNota)     //Nota Final
			MV_PAR03 := PadR(_cSerie, nTamSerie)    //SÈrie da Nota
			MV_PAR04 := 2                          //NF de Saida
			MV_PAR05 := 2                          //Frente e Verso = Nao
			MV_PAR06 := 2                          //DANFE simplificado = Nao
			MV_PAR07 := _dEmis
			MV_PAR08 := _dEmis

			aImpressora := GetImpWindows(.F.)

			IF Alltrim(aImpressora[1]) $ "Microsoft Print to PDF|PDFCreator|Cute PDF Writer|PDF"
				oDanfe := FWMSPrinter():New(cFilePrint, IMP_PDF, .F., , .T.)
				oDanfe:nDevice  := 6
			Else
				//oDanfe := FWMSPrinter():New(cFilePrint, IMP_SPOOL, .F., , .T.)
				oDanfe := FwMsPrinter():New(cFilePrint, IMP_SPOOL, .T.,,.T.,,,aImpressora[1])
				oDanfe:nDevice  := 2
			endIf
			oDanfe:SetResolution(78)
			oDanfe:SetPortrait()
			oDanfe:SetPaperSize(DMPAPER_A4)
			oDanfe:SetMargin(60, 60, 60, 60)

			//ForÁa a impress„o em PDF
//			oDanfe:nDevice  := 6
			oDanfe:cPathPDF := cPasta
			oDanfe:lServer  := .F.
			oDanfe:lViewPDF := .F. //.T.


			//Vari·veis obrigatÛrias da DANFE
			PixelX    := oDanfe:nLogPixelX()
			PixelY    := oDanfe:nLogPixelY()
			nConsNeg  := 0.4
			nConsTex  := 0.5
			oRetNF    := Nil
			lPtImpBol := .F.
			aNotasBol := {}
			nColAux   := 0

			//Chamando a impress„o da danfe no RDMAKE
			If RptStatus( {|lEnd| U_DANFEProc(@oDanfe, @lEnd, cIDEnt, Nil, Nil, .F., nil, nil)}, "Imprimindo DANFE..." )
				_lRet := .t.
				SF2->(dbsetorder(1))
				If SF2->(dbSeek(xFilial('SF2') + _cNota + _cSerie))
					Reclock("SF2",.f.)
					SF2->F2_ZIMP := 'S'
					MsUnlock("SF2")
				EndIf
			EndIf
			oDanfe:Print()
		Next
	EndIf

	FreeObj(oDanfe)
	oDanfe := Nil

	RestArea(aArea)

RETURN (_lRet)

//-----------------------------------
// Impressùo Boleto
//-----------------------------------
User Function 109Boleto(_cDoc,_cSerie)//U_XAG0045E('06','000003266','2')

	Local _aArea:= GetArea()

//  Acha nota fiscal
	sf2->(dbsetorder(1))
	sf2->(dbseek(xFilial("SF2")+_cDoc+_cSerie))
	_cCliente := sf2->F2_CLIENTE
	_cLoja    := sf2->f2_loja
	_cBoleto  := SuperGetMV( "MV_XBOLETO" , .F. , "S" ) //GETMV( MV_XBOLETO )

	if _cBoleto == "S"   // Devera imprimir boleto
		cRet := U_XAG0045E(cFilant , _cDoc, _cSerie , "I")
		if cRet <> "SUCESSO"
			MsgAlert("ATEN«√O: GeraÁ„o do Boleto com erro, verifique !!!","ERRO")
			_lRet := .f.
		else
			_lRet := .t.
		endif
	else
		MSGALERT( "Emiss„o Boleto n„o liberada. Favor acionar Suporte !!!", "AVISO" )
		_lRet := .f.
	endif

	RestArea(_aArea)

Return (_lRet)

//-----------------------------------
// Impressùo Romaneio
//-----------------------------------
User Function 109Romaneio(_cDoc, _cSerie)
	Local _aArea:= GetArea()
	Local _cRomaneio := SuperGetMV( "MV_XROMAN" , .F. , "N" ) //GETMV("MV_XROMAN")
	Local _cNota     := _cDoc

	if Empty(_cNota)
		MsgAlert('Nota Fiscal ainda nao gerada. Verifique!!', 'ATENCAO')
		_lRet := .f.
	else
		// chama rotin Romaneio
		if _cRomaneio == "S"
			u_AGR245()
			_lRet := .t.
			//else
			//	MSGALERT( "ExecuÁ„o Romaneio nao liberada. Verifique com o Suporte !!!", "AVISO" )
			//	_lRet := .f.
		endif
	endif

	RestArea(_aArea)

RETURN (_lRet)
