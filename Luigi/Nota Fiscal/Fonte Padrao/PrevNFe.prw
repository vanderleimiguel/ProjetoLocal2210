#INCLUDE "PROTHEUS.CH"
#INCLUDE "TBICONN.CH"                  
#INCLUDE "COLORS.CH"                      
#INCLUDE "RPTDEF.CH"
#INCLUDE "FWPrintSetup.ch"

#DEFINE RDDSPED "TOPCONN"
#DEFINE IMP_SPOOL 2

#DEFINE VBOX       080
#DEFINE VSPACE     008
#DEFINE HSPACE     010
#DEFINE SAYVSPACE  008
#DEFINE SAYHSPACE  008
#DEFINE HMARGEM    030
#DEFINE VMARGEM    030
#DEFINE MAXITEM    022                                                // Máximo de produtos para a primeira página 
#DEFINE MAXITEMP2  049                                                // Máximo de produtos para a pagina 2 em diante 
#DEFINE MAXITEMP2F 069                                                // Máximo de produtos para a página 2 em diante quando a página não possui informações complementares
#DEFINE MAXITEMP3  025                                                // Máximo de produtos para a pagina 2 em diante (caso utilize a opção de impressao em verso) - Tratamento implementado para atender a legislacao que determina que a segunda pagina de ocupar 50%.
#DEFINE MAXITEMC   050 //ANTES 42                                     // Máxima de caracteres por linha de produtos/serviços
#DEFINE MAXMENLIN  090                                                // Máximo de caracteres por linha de dados adicionais
#DEFINE MAXMSG     013                                                // Máximo de dados adicionais por página
#DEFINE MAXVALORC  018 //ANTES 12                                     // Máximo de caracteres por linha de valores numéricos

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  |PrevNFe   ºAutor  ³Bilhega, JC         º Data ³  07/05/10   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDesc.     ³ Pre-Visualiza Danfe sem enviar ao SEFAZ                    º±±
±±º          ³                                                            º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±³   DATA   ³ Programador   ³Manutencao efetuada                         ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³27/06/12  ³Daniel Peixoto ³SCXXXX - Criacao da Rotina                  ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/

User Function PrevNFe()
Local cAlias   := Alias()
Local cMail    := "", cSoapErro := "", cModelo := "", aRespNfe
Local cAviso   := "", cErro := ""
Local cStatus  := ""   
Local oObjSetup        
Local nCont    := 0, lExistNfe := .F.
      
Private cXML     := "",  oSetup,  aNotas   := {}, aNota := {}
Private cTipo    := IIF(ALIAS() == "SF2", "S", "E")   
Private nConsNeg := 0.4 // Constante para concertar o cálculo retornado pelo GetTextWidth para fontes em negrito.
Private nConsTex := 0.5 // Constante para concertar o cálculo retornado pelo GetTextWidth.
Private cNFMod := ""

 If !Pergunte("NFSIGW",.T.)
  Return(.F.)
 EndIf
 
 dbSelectArea("SF3")
	dbSetOrder(5)
	If MV_PAR04==1
		cWhere := "%SubString(SF3.F3_CFO,1,1) < '5' AND SF3.F3_FORMUL='S'%"
	ElseIf MV_PAR04==2
		cWhere := "%SubString(SF3.F3_CFO,1,1) >= '5'%"
	EndIf

	cAliasSF3 := GetNextAlias()
	lQuery    := .T.
			
	If Empty(cWhere)
		BeginSql Alias cAliasSF3
			COLUMN F3_ENTRADA AS DATE
			COLUMN F3_DTCANC AS DATE
				
			SELECT	F3_FILIAL,F3_ENTRADA,F3_NFELETR,F3_CFO,F3_FORMUL,F3_NFISCAL,F3_SERIE,F3_CLIEFOR,F3_LOJA,F3_ESPECIE,F3_DTCANC
			FROM %Table:SF3% SF3
			WHERE
			SF3.F3_FILIAL = %xFilial:SF3% AND
			SF3.F3_SERIE = %Exp:MV_PAR03% AND
			SF3.F3_NFISCAL >= %Exp:MV_PAR01% AND
			SF3.F3_NFISCAL <= %Exp:MV_PAR02% AND
			SF3.F3_DTCANC = %Exp:Space(8)% AND  // // Roberto Fiuza imprimir nf cancelada
			SF3.%notdel%
		EndSql
				
	Else
		BeginSql Alias cAliasSF3
			COLUMN F3_ENTRADA AS DATE
			COLUMN F3_DTCANC AS DATE
				
			SELECT	F3_FILIAL,F3_ENTRADA,F3_NFELETR,F3_CFO,F3_FORMUL,F3_NFISCAL,F3_SERIE,F3_CLIEFOR,F3_LOJA,F3_ESPECIE,F3_DTCANC
			FROM %Table:SF3% SF3
			WHERE
			SF3.F3_FILIAL = %xFilial:SF3% AND
			SF3.F3_SERIE = %Exp:MV_PAR03% AND
			SF3.F3_NFISCAL >= %Exp:MV_PAR01% AND
			SF3.F3_NFISCAL <= %Exp:MV_PAR02% AND
			%Exp:cWhere% AND
			SF3.F3_DTCANC = %Exp:Space(8)% AND  // // Roberto Fiuza imprimir nf cancelada
			SF3.%notdel%
		EndSql
		
	EndIf
			
	If MV_PAR04==1
		cWhere := "SubStr(F3_CFO,1,1) < '5' .AND. F3_FORMUL=='S'"
	Elseif MV_PAR04==2
		cWhere := "SubStr(F3_CFO,1,1) >= '5'"
	Else
		cWhere := ".T."
	EndIf
		
	While !Eof() .And. xFilial("SF3") == (cAliasSF3)->F3_FILIAL .And.;
	     	(cAliasSF3)->F3_SERIE == MV_PAR03 .And.;
    			(cAliasSF3)->F3_NFISCAL >= MV_PAR01 .And.;
    			(cAliasSF3)->F3_NFISCAL <= MV_PAR02
			
		dbSelectArea(cAliasSF3)
		If  Empty((cAliasSF3)->F3_DTCANC) .And. &cWhere // Roberto Fiuza imprimir nf cancelada
//		If   &cWhere 
			
			If (SubStr((cAliasSF3)->F3_CFO,1,1)>="5" .Or. (cAliasSF3)->F3_FORMUL=="S") .And. aScan(aNotas,{|x| x[4]+x[5]+x[6]+x[7]==(cAliasSF3)->F3_SERIE+(cAliasSF3)->F3_NFISCAL+(cAliasSF3)->F3_CLIEFOR+(cAliasSF3)->F3_LOJA})==0
  	         cStatus := IIF((cAliasSF3)->F3_CFO<"5", SF1->F1_FIMP, SF2->F2_FIMP)
  	         If cStatus == "S"
   		      //dbSkip()
  	            //Loop         
  	          EndIf

         	 aadd(aNotas, {} )

				aadd(Atail(aNotas),IIF((cAliasSF3)->F3_CFO<"5", "0", "1"))
				aadd(Atail(aNotas),(cAliasSF3)->F3_ENTRADA)
				aadd(Atail(aNotas),(cAliasSF3)->F3_SERIE)
				aadd(Atail(aNotas),(cAliasSF3)->F3_NFISCAL)
				aadd(Atail(aNotas),(cAliasSF3)->F3_CLIEFOR)
				aadd(Atail(aNotas),(cAliasSF3)->F3_LOJA)
				aadd(Atail(aNotas),"")
 		   EndIf		
		EndIf
			
		dbSelectArea(cAliasSF3)
		dbSkip()
	EndDo
   
 If Empty(aNotas)         
  Alert("Nenhuma Nota Encontrada!!  Verifique se a Nota existe ou já foi autorizada.")   
  Return .F.
 ENDIF

	//**** INICIALIZACAO DO OBJETO DE IMPRESSAO - APENAS POR PDF ****//
	oDanfe 	:= FWMSPrinter():New("DANFE" , IMP_PDF, .F.,, .T.)   
	

 oDanfe:SetResolution(78) //Tamanho estipulado para a Danfe
 oDanfe:SetPortrait()
 oDanfe:SetPaperSize(DMPAPER_A4)
 oDanfe:SetMargin(60,60,60,60)
   
    // ----------------------------------------------
	// Define saida de impressão
	// ----------------------------------------------
	oDanfe:nDevice := IMP_PDF
	// ----------------------------------------------
	// Define para salvar o PDF
	// ----------------------------------------------
   

//	oDanfe:cPathPDF := "c:\"      
	oDanfe:cPathPDF := "C:\relatorios totvs\Danfe\"      // ROBERTO FIUZA TESTE 

 For nCont := 1 To Len(aNotas)
	 //Array para geracao do XML
	 aNota := {}
	 aadd(aNota, aNotas[nCont] )
  aadd(aNota, "2.00")
  aadd(aNota, "2")
  aadd(aNota, {"", ""})
 
  //User Function XmlNfeSef(cTipo,cSerie,cNota,cClieFor,cLoja,cNotaOri,cSerieOri)
  aXml := ExecBlock("XmlNFeSef",.F.,.F.,aNota)	 
  cXML := aXML[2]
	  
  //Aviso("Erro Pre-XML",@cXML,{"Sair"},3,/*cCaption2*/,/*nRotAutDefault*/,/*cBitmap*/,.T.)
	  
  // Estrutura SPED000
  //   ID_ENT 			C(006)
  //		 PARAMETRO C(010)                                     
  //   CONTEUDO		C(100)
  //   Adicionar as linhas
  //   ID_ENT  PARAMETRO  CONTEUDO
  //   000000  MV_VERSAO  2.00
  //			000000  MV_MODALID 1
  //			000000  MV_AMBIENT 2
 
  // Simular ambiente TSS NF-e
   cIDEnt    := "000001"
//  cArquivo := "/system/sped000.dbf"
   cAlias   := "SPED000"
//  dbUseArea(.T.,"DBFCDXADS",cArquivo,cAlias)
    cVersao   := "2.00"   
    cModalidad:= "1"
    cAmbiente := "2"
                      
 /* cIdEnt   := "000000"
  cAlias   := "SPED000"
  OpenTable(cAlias)*/
    
  IF !SpedNfeConv(@cXML,cIDEnt,@cMail,,@cSoapErro,@cModelo,@aRespNfe)
   //Alert("Erro :" + cSoapErro)
   Aviso("Erro Pre-XML",cSoapErro,{"Sair"},3,/*cCaption2*/,/*nRotAutDefault*/,/*cBitmap*/,.T.)
   DBCloseArea(cAlias)
   FreeObj(oDanfe)
   oDanfe := Nil
   Return .F.
  ENDIF
                      
  DBCloseArea(cAlias)

	 //Array para impressao DANFE
	 aNota := {}
  aadd(aNota,.F.)
 	aadd(aNota,IIF(aNotas[1][1]=="0","E","S"))
		aadd(aNota,aNotas[1][2])
		aadd(aNota,aNotas[1][3])
		aadd(aNota,aNotas[1][4])
		aadd(aNota,aNotas[1][5])
		aadd(aNota,aNotas[1][6])

  lExistNfe := PreDanfe(cIdEnt,,,oDanfe,oSetup,oDanfe:cFilePrint, aNota)
 Next  

 If lExistNfe
 	oDanfe:Preview(AllTrim(GetProfString(GetPrinterSession(),"DEFAULT","",.T.)))
 Else
 	Aviso("DANFE","Nenhuma NF-e a ser impressa nos parametros utilizados.",{"OK"},3)
 EndIf 
 FreeObj(oDanfe)
 oDanfe := Nil
 
Return .T.

//Static Function PrtNfeSef(cIdEnt,cVal1,cVal2,oDanfe,oSetup,cFilePrint)
Static Function PreDanfe(cIdEnt,cVal1,cVal2,oDanfe,oSetup,cFilePrint, aNota)
Local aArea     := GetArea()
Local lExistNfe := .F.
Local lEnd      := .F.

Private PixelX := odanfe:nLogPixelX()
Private PixelY := odanfe:nLogPixelY()
Private nNroLinAd := MAXMSG

	// ------------------------------------------------------------------------------------------
	// Execução via thread de smartclient
	// ------------------------------------------------------------------------------------------
 //RptStatus({|lEnd| DanfeProc(@oDanfe,@lEnd,cIdEnt,,,@lExistNfe)},"Imprimindo Danfe...")
	DanfeProc(@oDanfe,@lEnd,cIdEnt,,,@lExistNfe, aNota)

 RestArea(aArea)
Return(lExistNfe)

Static Function DanfeProc(oDanfe,lEnd,cIdEnt,cVal1,cVal2,lExistNfe, aNota)
Local cAviso := "", cErro := ""
 
	oNfe := XmlParser(cXML,"_",@cAviso,@cErro)					
		
	If Empty(cAviso) .And. Empty(cErro)	                                                 
		lExistNfe := .T.

//ImpDet(@oDanfe,oNFe, cAutoriza, cModalidade, oNfeDPEC, cCodAutDPEC, aXml[nX][6], aXml[nX][7])
		ImpDet(@oDanfe,oNFe, ""      , ""         ,         , ""         , ""         , ""            , aNota )
	EndIf

Return (.T.)

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Program   ³ ImpDet   ³ Autor ³ Eduardo Riera         ³ Data ³16.11.2006³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡…o ³Controle de Fluxo do Relatorio.                             ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Retorno   ³Nenhum                                                      ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Parametros³ExpO1: Objeto grafico de impressao                    (OPC) ³±±
±±³          ³ExpC2: String com o XML da NFe                              ³±±
±±³          ³ExpC3: Codigo de Autorizacao do fiscal                (OPC) ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³   DATA   ³ Programador   ³Manutencao efetuada                         ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³          ³               ³                                            ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/
Static Function ImpDet(oDanfe,oNfe,cCodAutSef,cModalidade,oNfeDPEC,cCodAutDPEC,cDtHrRecCab,dDtReceb,aNota)

PRIVATE oFont10N   := TFontEx():New(oDanfe,"Times New Roman",08,08,.T.,.T.,.F.)// 1
PRIVATE oFont07N   := TFontEx():New(oDanfe,"Times New Roman",06,06,.T.,.T.,.F.)// 2
PRIVATE oFont07    := TFontEx():New(oDanfe,"Times New Roman",06,06,.F.,.T.,.F.)// 3
PRIVATE oFont08    := TFontEx():New(oDanfe,"Times New Roman",07,07,.F.,.T.,.F.)// 4
PRIVATE oFont08N   := TFontEx():New(oDanfe,"Times New Roman",06,06,.T.,.T.,.F.)// 5
PRIVATE oFont09N   := TFontEx():New(oDanfe,"Times New Roman",08,08,.T.,.T.,.F.)// 6
PRIVATE oFont09    := TFontEx():New(oDanfe,"Times New Roman",08,08,.F.,.T.,.F.)// 7
PRIVATE oFont10    := TFontEx():New(oDanfe,"Times New Roman",09,09,.F.,.T.,.F.)// 8
PRIVATE oFont11    := TFontEx():New(oDanfe,"Times New Roman",10,10,.F.,.T.,.F.)// 9
PRIVATE oFont12    := TFontEx():New(oDanfe,"Times New Roman",11,11,.F.,.T.,.F.)// 10
PRIVATE oFont11N   := TFontEx():New(oDanfe,"Times New Roman",10,10,.T.,.T.,.F.)// 11
PRIVATE oFont18N   := TFontEx():New(oDanfe,"Times New Roman",17,17,.T.,.T.,.F.)// 12 
PRIVATE OFONT12N   := TFontEx():New(oDanfe,"Times New Roman",11,11,.T.,.T.,.F.)// 12  

PrtDanfe(@oDanfe,oNfe,cCodAutSef,cModalidade,oNfeDPEC,cCodAutDPEC,cDtHrRecCab,dDtReceb,aNota)

Return(.T.)


/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Fun‡…o    ³PrtDanfe  ³ Autor ³Eduardo Riera          ³ Data ³16.11.2006³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡…o ³Impressao do formulario DANFE grafico conforme laytout no   ³±±
±±³          ³formato retrato                                             ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Sintaxe   ³ PrtDanfe()                                                 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Retorno   ³ Nenhum                                                     ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Parametros³ExpO1: Objeto grafico de impressao                          ³±±
±±³          ³ExpO2: Objeto da NFe                                        ³±±
±±³          ³ExpC3: Codigo de Autorizacao do fiscal                (OPC) ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³   DATA   ³ Programador   ³Manutencao Efetuada                         ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³          ³               ³                                            ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
Static Function PrtDanfe(oDanfe,oNFE,cCodAutSef,cModalidade,oNfeDPEC,cCodAutDPEC,cDtHrRecCab,dDtReceb,aNota)

Local aAuxCabec     := {} // Array que conterá as strings de cabeçalho das colunas de produtos/serviços.
Local aTamanho      := {}
Local aTamCol       := {} // Array que conterá o tamanho das colunas dos produtos/serviços.
Local aSitTrib      := {}
Local aSitSN		:= {}
Local aTransp       := {}
Local aDest         := {}
Local aHrEnt 		:= {}
Local aFaturas      := {}
Local aItens        := {}
Local aISSQN        := {}
Local aSimpNac		:= {}
Local aTotais       := {}
Local aAux          := {}
Local aUF           := {}
Local aMensagem     := {}
Local aEspVol       := {}
Local aResFisco     := {} 
Local aEspecie      := {}
Local aIndImp	    := {}
Local aIndAux	    := {} 
Local aLote         := {}  

Local nHPage        := 0
Local nVPage        := 0
Local nPosV         := 0
Local nPosVOld      := 0
Local nPosH         := 0
Local nPosHOld      := 0
Local nAuxH         := 0
Local nAuxH2        := 0
Local nAuxV         := 0
Local nX            := 0
Local nY            := 0
Local nL            := 0
Local nJ            := 0
Local nW            := 0
Local nTamanho      := 0
Local nFolha        := 1
Local nFolhas       := 0
Local nItem         := 0
Local nMensagem     := 0
Local nBaseICM      := 0
Local nValICM       := 0
Local nValIPI       := 0
Local nPICM         := 0
Local nPIPI         := 0
Local nFaturas      := 0
Local nVTotal       := 0
Local nQtd          := 0
Local nVUnit        := 0
Local nVolume	    := 0
Local nLenFatura
Local nLenVol
Local nLenDet
Local nLenSit
Local nLenItens     := 0
Local nLenMensagens := 0
Local nLen          := 0
Local nColuna	    := 0
Local nLinSum	    := 0
Local nE		    := 0
Local nPag
Local nItensRes
Local nSoma       
Local nZ		    := 0 
Local nMaxCod	    := 10
Local nMaxDes	    := MAXITEMC 
Local nLinhavers    := 0
Local nMaxItemP2    := MAXITEM // Variável utilizada para tratamento de quantos itens devem ser impressos na página corrente 

Local cAux          := ""
Local cSitTrib      := ""
Local cUF		 	:= ""  
Local cMVCODREG		:= SuperGetMV("MV_CODREG", ," ")
Local cChaveCont 	:= ""
Local cLogo      	:= SuperGetMV("MV_LOGONFE")//FisxLogo("1")
Local cGuarda       := ""  
Local cEsp		    := "" 
Local cLogoD	    := ""
local cEndDest      := ""

Local lPreview      := .F.
Local lFlag         := .T.
Local lConverte     := GetNewPar("MV_CONVERT",.F.)
Local lImpAnfav     := GetNewPar("MV_IMPANF",.F.)
Local lImpInfAd   	:= GetNewPar("MV_IMPADIC",.F.)
Local lImpSimpN		:= GetNewPar("MV_IMPSIMP",.F.)
Local lPagPar
Local lMv_Logod     := If(GetNewPar("MV_LOGOD", "N" ) == "S", .T., .F.   )
Local lMv_ItDesc    := Iif( GetNewPar("MV_ITDESC","N")=="S", .T., .F. )
Local lNFori2 	    := .T.
Local lFimpar	    := .T. 	                     
Local lCompleECF    := .F.
Local lEntIpiDev   	:= GetNewPar("MV_EIPIDEV",.F.) /*Apenas para nota de entrada de Devolução de ipi. .T.-Séra destacado no cabeçalho + inf.compl/.F.-Será destacado apenas em inf.compl*/



Default cDtHrRecCab := ""
Default dDtReceb    := CToD("")

Private aInfNf    := {}

Private oDPEC     := oNfeDPEC
Private oNF       := oNFe:_NFe
Private oEmitente := oNF:_InfNfe:_Emit
Private oIdent    := oNF:_InfNfe:_IDE
Private oDestino  := oNF:_InfNfe:_Dest
Private oTotal    := oNF:_InfNfe:_Total
Private oTransp   := oNF:_InfNfe:_Transp
Private oDet      := oNF:_InfNfe:_Det
Private oFatura   := IIf(Type("oNF:_InfNfe:_Cobr")=="U",Nil,oNF:_InfNfe:_Cobr)
Private oImposto

Private nPrivate  := 0
Private nPrivate2 := 0
Private nXAux	  := 0

Private lArt488MG := .F.
Private lArt274SP := .F. 

//Private cMVTesImpD := GetMV("MV_TESIMPD")

nFaturas := IIf(oFatura<>Nil,IIf(ValType(oNF:_InfNfe:_Cobr:_Dup)=="A",Len(oNF:_InfNfe:_Cobr:_Dup),1),0)
oDet := IIf(ValType(oDet)=="O",{oDet},oDet)
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Carrega as variaveis de impressao                                       ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
aadd(aSitTrib,"00")
aadd(aSitTrib,"10")
aadd(aSitTrib,"20")
aadd(aSitTrib,"30")
aadd(aSitTrib,"40")
aadd(aSitTrib,"41")
aadd(aSitTrib,"50")
aadd(aSitTrib,"51")
aadd(aSitTrib,"60")
aadd(aSitTrib,"70")
aadd(aSitTrib,"90")

aadd(aSitSN,"101")
aadd(aSitSN,"102")
aadd(aSitSN,"201")
aadd(aSitSN,"202")
aadd(aSitSN,"500")
aadd(aSitSN,"900")

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Quadro Destinatario                                                     ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

cEndDest := NoChar(oDestino:_EnderDest:_Xlgr:Text,lConverte)
If  " SN" $ (UPPER (oDestino:_EnderDest:_Xlgr:Text)) .Or. ",SN" $ (UPPER (oDestino:_EnderDest:_Xlgr:Text)) .Or. ;
    "S/N" $ (UPPER (oDestino:_EnderDest:_Xlgr:Text)) 
   
            cEndDest += IIf(Type("oDestino:_EnderDest:_xcpl")=="U","",", " + NoChar(oDestino:_EnderDest:_xcpl:Text,lConverte))
Else
            cEndDest += +","+NoChar(oDestino:_EnderDest:_NRO:Text,lConverte) + IIf(Type("oDestino:_EnderDest:_xcpl")=="U","",", "+ NoChar(oDestino:_EnderDest:_xcpl:Text,lConverte))
Endif   

aDest := {cEndDest,;
NoChar(oDestino:_EnderDest:_XBairro:Text,lConverte),;
IIF(Type("oDestino:_EnderDest:_Cep")=="U","",Transform(oDestino:_EnderDest:_Cep:Text,"@r 99999-999")),;
IIF(Type("oIdent:_DSaiEnt")=="U","",oIdent:_DSaiEnt:Text),;//                              oIdent:_DSaiEnt:Text,;
oDestino:_EnderDest:_XMun:Text,;
IIF(Type("oDestino:_EnderDest:_fone")=="U","",oDestino:_EnderDest:_fone:Text),;
oDestino:_EnderDest:_UF:Text,;
oDestino:_IE:Text,;
""}

If Type("oIdent:_DSaiEnt")<>"U" .And. Type("oIdent:_HSaiEnt:Text")<>"U"
	aAdd(aHrEnt,oIdent:_HSaiEnt:Text)
Else
	aAdd(aHrEnt,"")
EndIf


//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Calculo do Imposto                                                      ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

//SC002425
lImprImp := .F.
If /*SFT->FT_TIPOMOV*/aNota[2] =="E"
 SD1->(dbSetOrder(1))
 SD1->(DbSeek(xFilial("SD1")+SF1->F1_DOC+SF1->F1_SERIE+SF1->F1_FORNECE+SF1->F1_LOJA))

 SF4->(dbSetOrder(1))
 SF4->( DbSeek( xFilial("SF4")+SD1->D1_TES))
 lImprImp := SF4->F4_CODIGO $ cMVTesImpD
EndIf

aTotais := {"","","","","","","","","","",""}
aTotais[01] := Transform(Val(oTotal:_ICMSTOT:_vBC:TEXT),"@ze 9,999,999,999,999.99")
If lImprImp
 aTotais[02] := "DIFERIDO"
Else
 aTotais[02] := Transform(Val(oTotal:_ICMSTOT:_vICMS:TEXT),"@ze 9,999,999,999,999.99")
EndIf 
aTotais[03] := Transform(Val(oTotal:_ICMSTOT:_vBCST:TEXT),"@ze 9,999,999,999,999.99")
aTotais[04] := Transform(Val(oTotal:_ICMSTOT:_vST:TEXT),"@ze 9,999,999,999,999.99")
aTotais[05] := Transform(Val(oTotal:_ICMSTOT:_vProd:TEXT),"@ze 9,999,999,999,999.99")
aTotais[06] := Transform(Val(oTotal:_ICMSTOT:_vFrete:TEXT),"@ze 9,999,999,999,999.99")
aTotais[07] := Transform(Val(oTotal:_ICMSTOT:_vSeg:TEXT),"@ze 9,999,999,999,999.99")
aTotais[08] := Transform(Val(oTotal:_ICMSTOT:_vDesc:TEXT),"@ze 9,999,999,999,999.99")
aTotais[09] := IIF(lImprImp .And. Val(oTotal:_ICMSTOT:_vOutro:TEXT) > 0, "I.I. ", "") + Transform(Val(oTotal:_ICMSTOT:_vOutro:TEXT),"@ze 9,999,999,999,999.99")

If ( MV_PAR04 == 1 )
	dbSelectArea("SF1")
	dbSetOrder(1)
	If MsSeek(xFilial("SF1")+aNota[5]+aNota[4]+aNota[6]+aNota[7]) .And. SF1->(FieldPos("F1_FIMP"))<>0
		If SF1->F1_TIPO <> "D"
		  	aTotais[10] := 	Transform(Val(oTotal:_ICMSTOT:_vIPI:TEXT),"@ze 9,999,999,999,999.99")
		ElseIf SF1->F1_TIPO == "D" .and. lEntIpiDev
			aTotais[10] := 	Transform(Val(oTotal:_ICMSTOT:_vIPI:TEXT),"@ze 9,999,999,999,999.99")
		Else	
			aTotais[10] := ""
		EndIf        
		MsUnlock()
		DbSkip()
	EndIf
Else
	aTotais[10] := 	Transform(Val(oTotal:_ICMSTOT:_vIPI:TEXT),"@ze 9,999,999,999,999.99")
EndIf
	

aTotais[11] := 	Transform(Val(oTotal:_ICMSTOT:_vNF:TEXT),"@ze 9,999,999,999,999.99")

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Impressão da Base de Calculo e ICMS nos campo Proprios do ICMS quando optante pelo Simples Nacional    ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
 
If !Empty("oDet[nX]:_IMPOSTO:_ICMS:_ICMSSN101:_VCREDICMSSN:TEXT") .And. lImpSimpN 

	aSimpNac := {"",""} 
	dbSelectArea("SF3") 
	dbSetOrder(5)
	If MsSeek(xFilial("SF3")+aNota[4]+aNota[5])	
		aSimpNac[01] := Transform((SF3->F3_BASEICM),"@ze 9,999,999,999,999.99")
		aSimpNac[02] := Transform((SF3->F3_VALICM),"@ze 9,999,999,999,999.99")
	EndIf
EndIf


//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Quadro Faturas                                                          ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
If nFaturas > 0
	For nX := 1 To 3
		aAux := {}
		For nY := 1 To Min(9, nFaturas)
			Do Case
				Case nX == 1
					If nFaturas > 1
						AAdd(aAux, AllTrim(oFatura:_Dup[nY]:_nDup:TEXT))
					Else
						AAdd(aAux, AllTrim(oFatura:_Dup:_nDup:TEXT))
					EndIf
				Case nX == 2
					If nFaturas > 1
						AAdd(aAux, AllTrim(ConvDate(oFatura:_Dup[nY]:_dVenc:TEXT)))
					Else
						AAdd(aAux, AllTrim(ConvDate(oFatura:_Dup:_dVenc:TEXT)))
					EndIf
				Case nX == 3
					If nFaturas > 1
						AAdd(aAux, AllTrim(TransForm(Val(oFatura:_Dup[nY]:_vDup:TEXT), "@E 9,999,999,999,999.99")))
					Else
						AAdd(aAux, AllTrim(TransForm(Val(oFatura:_Dup:_vDup:TEXT), "@E 9,999,999,999,999.99")))
					EndIf
			EndCase
		Next nY
		If nY <= 9
			For nY := 1 To 9
				AAdd(aAux, Space(20))
			Next nY
		EndIf
		AAdd(aFaturas, aAux)
	Next nX
EndIf

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Quadro transportadora                                                   ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
aTransp := {"","0","","","","","","","","","","","","","",""}

If Type("oTransp:_ModFrete")<>"U"
	aTransp[02] := IIF(Type("oTransp:_ModFrete:TEXT")<>"U",oTransp:_ModFrete:TEXT,"0")
EndIf
If Type("oTransp:_Transporta")<>"U"
	aTransp[01] := IIf(Type("oTransp:_Transporta:_xNome:TEXT")<>"U",NoChar(oTransp:_Transporta:_xNome:TEXT,lConverte),"")
	//	aTransp[02] := IIF(Type("oTransp:_ModFrete:TEXT")<>"U",oTransp:_ModFrete:TEXT,"0")
	aTransp[03] := IIf(Type("oTransp:_VeicTransp:_RNTC")=="U","",oTransp:_VeicTransp:_RNTC:TEXT)
	aTransp[04] := IIf(Type("oTransp:_VeicTransp:_Placa:TEXT")<>"U",oTransp:_VeicTransp:_Placa:TEXT,"")
	aTransp[05] := IIf(Type("oTransp:_VeicTransp:_UF:TEXT")<>"U",oTransp:_VeicTransp:_UF:TEXT,"")
	If Type("oTransp:_Transporta:_CNPJ:TEXT")<>"U"
		aTransp[06] := Transform(oTransp:_Transporta:_CNPJ:TEXT,"@r 99.999.999/9999-99")
	ElseIf Type("oTransp:_Transporta:_CPF:TEXT")<>"U"
		aTransp[06] := Transform(oTransp:_Transporta:_CPF:TEXT,"@r 999.999.999-99")
	EndIf
	aTransp[07] := IIf(Type("oTransp:_Transporta:_xEnder:TEXT")<>"U",NoChar(oTransp:_Transporta:_xEnder:TEXT,lConverte),"")
	aTransp[08] := IIf(Type("oTransp:_Transporta:_xMun:TEXT")<>"U",oTransp:_Transporta:_xMun:TEXT,"")
	aTransp[09] := IIf(Type("oTransp:_Transporta:_UF:TEXT")<>"U",oTransp:_Transporta:_UF:TEXT,"")
	aTransp[10] := IIf(Type("oTransp:_Transporta:_IE:TEXT")<>"U",oTransp:_Transporta:_IE:TEXT,"")
ElseIf Type("oTransp:_VEICTRANSP")<>"U"
	aTransp[03] := IIf(Type("oTransp:_VeicTransp:_RNTC")=="U","",oTransp:_VeicTransp:_RNTC:TEXT)
	aTransp[04] := IIf(Type("oTransp:_VeicTransp:_Placa:TEXT")<>"U",oTransp:_VeicTransp:_Placa:TEXT,"")
	aTransp[05] := IIf(Type("oTransp:_VeicTransp:_UF:TEXT")<>"U",oTransp:_VeicTransp:_UF:TEXT,"")
EndIf
If Type("oTransp:_Vol")<>"U"
	If ValType(oTransp:_Vol) == "A"
		nX := nPrivate
		nLenVol := Len(oTransp:_Vol)
		For nX := 1 to nLenVol
			nXAux := nX
			nVolume += IIF(!Type("oTransp:_Vol[nXAux]:_QVOL:TEXT")=="U",Val(oTransp:_Vol[nXAux]:_QVOL:TEXT),0)
		Next nX
		aTransp[11]	:= AllTrim(str(nVolume))
		aTransp[12]	:= IIf(Type("oTransp:_Vol:_Esp")=="U","Diversos","")
		aTransp[13] := IIf(Type("oTransp:_Vol:_Marca")=="U","",NoChar(oTransp:_Vol:_Marca:TEXT,lConverte))
		aTransp[14] := IIf(Type("oTransp:_Vol:_nVol:TEXT")<>"U",oTransp:_Vol:_nVol:TEXT,"")
		If  Type("oTransp:_Vol[1]:_PesoB") <>"U"
			nPesoB := Val(oTransp:_Vol[1]:_PesoB:TEXT)
			aTransp[15] := AllTrim(str(nPesoB))
		EndIf
		If Type("oTransp:_Vol[1]:_PesoL") <>"U"
			nPesoL := Val(oTransp:_Vol[1]:_PesoL:TEXT)
			aTransp[16] := AllTrim(str(nPesoL))
		EndIf
	Else
		aTransp[11] := IIf(Type("oTransp:_Vol:_qVol:TEXT")<>"U",oTransp:_Vol:_qVol:TEXT,"")
		aTransp[12] := IIf(Type("oTransp:_Vol:_Esp")=="U","",oTransp:_Vol:_Esp:TEXT)
		aTransp[13] := IIf(Type("oTransp:_Vol:_Marca")=="U","",NoChar(oTransp:_Vol:_Marca:TEXT,lConverte))
		aTransp[14] := IIf(Type("oTransp:_Vol:_nVol:TEXT")<>"U",oTransp:_Vol:_nVol:TEXT,"")
		aTransp[15] := IIf(Type("oTransp:_Vol:_PesoB:TEXT")<>"U",oTransp:_Vol:_PesoB:TEXT,"")
		aTransp[16] := IIf(Type("oTransp:_Vol:_PesoL:TEXT")<>"U",oTransp:_Vol:_PesoL:TEXT,"")
	EndIf
	aTransp[15] := strTRan(aTransp[15],".",",")
	aTransp[16] := strTRan(aTransp[16],".",",")
EndIf

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Volumes / Especie Nota de Saida                                         ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
If(MV_PAR04==2) .And. Empty(aTransp[12])	

	If (SF2->(FieldPos("F2_ESPECI1")) <>0 .And. !Empty( SF2->(FieldGet(FieldPos( "F2_ESPECI1" )))  )) .Or.;
		(SF2->(FieldPos("F2_ESPECI2")) <>0 .And. !Empty( SF2->(FieldGet(FieldPos( "F2_ESPECI2" )))  )) .Or.;
		(SF2->(FieldPos("F2_ESPECI3")) <>0 .And. !Empty( SF2->(FieldGet(FieldPos( "F2_ESPECI3" )))  )) .Or.;
		(SF2->(FieldPos("F2_ESPECI4")) <>0 .And. !Empty( SF2->(FieldGet(FieldPos( "F2_ESPECI4" )))  ))
		
		aEspecie := {}
		aadd(aEspecie,SF2->F2_ESPECI1)
		aadd(aEspecie,SF2->F2_ESPECI2)
		aadd(aEspecie,SF2->F2_ESPECI3)
		aadd(aEspecie,SF2->F2_ESPECI4)
		
		cEsp := ""
		nx 	 := 0
		For nE := 1 To Len(aEspecie)
			If !Empty(aEspecie[nE])
				nx ++
				cEsp := aEspecie[nE]
			EndIf
		Next
		
		cGuarda := ""
		If nx > 1
			cGuarda := "Diversos"
		Else
			cGuarda := cEsp
		EndIf
		
		If !Empty(cGuarda)
			aadd(aEspVol,{cGuarda,Iif(SF2->F2_PLIQUI>0,str(SF2->F2_PLIQUI),""),Iif(SF2->F2_PBRUTO>0, str(SF2->F2_PBRUTO),"")})
		Else
			/*
			//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ1
			//³Aqui seguindo a mesma regra da criação da TAG de Volumes no xml  ³
			//³ caso não esteja preenchida nenhuma das especies de Volume não se³
			//³ envia as informações de volume.                   				³
			//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ1
			*/
			aadd(aEspVol,{cGuarda,"",""})
		Endif
	Else
		aadd(aEspVol,{cGuarda,"",""})
	EndIf
EndIf
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Especie Nota de Entrada                                                 ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
If(MV_PAR04==1) .And. Empty(aTransp[12])
	If (SF1->(FieldPos("F1_ESPECI1")) <>0 .And. !Empty( SF1->(FieldGet(FieldPos( "F1_ESPECI1" )))  )) .Or.;
		(SF1->(FieldPos("F1_ESPECI2")) <>0 .And. !Empty( SF1->(FieldGet(FieldPos( "F1_ESPECI2" )))  )) .Or.;
		(SF1->(FieldPos("F1_ESPECI3")) <>0 .And. !Empty( SF1->(FieldGet(FieldPos( "F1_ESPECI3" )))  )) .Or.;
		(SF1->(FieldPos("F1_ESPECI4")) <>0 .And. !Empty( SF1->(FieldGet(FieldPos( "F1_ESPECI4" )))  ))
		
		aEspecie := {}
		aadd(aEspecie,SF1->F1_ESPECI1)
		aadd(aEspecie,SF1->F1_ESPECI2)
		aadd(aEspecie,SF1->F1_ESPECI3)
		aadd(aEspecie,SF1->F1_ESPECI4)
		
		cEsp := ""
		nx 	 := 0
		For nE := 1 To Len(aEspecie)
			If !Empty(aEspecie[nE])
				nx ++
				cEsp := aEspecie[nE]
			EndIf
		Next
		
		cGuarda := ""
		If nx > 1
			cGuarda := "Diversos"
		Else
			cGuarda := cEsp
		EndIf
		
		If  !Empty(cGuarda)
			aadd(aEspVol,{cGuarda,Iif(SF1->F1_PLIQUI>0,str(SF1->F1_PLIQUI),""),Iif(SF1->F1_PBRUTO>0, str(SF1->F1_PBRUTO),"")})
		Else
			/*
			//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ1
			//³Aqui seguindo a mesma regra da criação da TAG de Volumes no xml  ³
			//³ caso não esteja preenchida nenhuma das especies de Volume não se³
			//³ envia as informações de volume.                   				³
			//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ1
			*/
			aadd(aEspVol,{cGuarda,"",""})
		Endif
	Else
		aadd(aEspVol,{cGuarda,"",""})
	EndIf
EndIf

//ÚÄ-----ÄÄÄÄÄÄÄÄÄÄÄ¿
//³Tipo do frete    ³
//ÀÄÄÄÄÄÄ-----ÄÄÄÄÄÄÙ
dbSelectArea("SD2")
dbSetOrder(3)
MsSeek(xFilial("SD2")+SF2->F2_DOC+SF2->F2_SERIE+SF2->F2_CLIENTE+SF2->F2_LOJA)
dbSelectArea("SC5")
dbSetOrder(1)
MsSeek(xFilial("SC5")+SD2->D2_PEDIDO)
dbSelectArea("SF4")
dbSetOrder(1)
MsSeek(xFilial("SF4")+SD2->D2_TES)
dbSelectArea("SF3")
dbSetOrder(4)
MsSeek(xFilial("SF3")+SF2->F2_CLIENTE+SF2->F2_LOJA+SF2->F2_DOC+SF2->F2_SERIE)

lArt488MG := Iif(SF4->(FIELDPOS("F4_CRLEIT"))>0,Iif(SF4->F4_CRLEIT == "1",.T.,.F.),.F.)
lArt274SP := Iif(SF4->(FIELDPOS("F4_ART274"))>0,Iif(SF4->F4_ART274 $ "1S",.T.,.F.),.F.)

If Type("oTransp:_ModFrete") <> "U"
	cModFrete := oTransp:_ModFrete:TEXT
Else
	cModFrete := "1"
EndIf

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Quadro Dados do Produto / Serviço                                       ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
nLenDet := Len(oDet)
If lMv_ItDesc
	For nX := 1 To nLenDet
		Aadd(aIndAux, {nX, SubStr(NoChar(oDet[nX]:_Prod:_xProd:TEXT,lConverte),1,MAXITEMC)})
	Next
	
	aIndAux := aSort(aIndAux,,, { |x, y| x[2] < y[2] })
	
	For nX := 1 To nLenDet
		Aadd(aIndImp, aIndAux[nX][1] )
	Next
EndIf

For nZ := 1 To nLenDet
	If lMv_ItDesc
		nX := aIndImp[nZ]
	Else
		nX := nZ
	EndIf
	nPrivate := nX
    If lArt488MG .And. SuperGetMv("MV_ESTADO")$"MG"
        nVTotal  := 0
        nVUnit   := 0 
    Else
	    nVTotal  := Val(oDet[nX]:_Prod:_vProd:TEXT)//-Val(IIF(Type("oDet[nPrivate]:_Prod:_vDesc")=="U","",oDet[nX]:_Prod:_vDesc:TEXT))
	    nVUnit   := Val(oDet[nX]:_Prod:_vUnCom:TEXT)
	EndIf
	nQtd     := Val(oDet[nX]:_Prod:_qTrib:TEXT)
	nBaseICM := 0
	nValICM  := 0
	nValIPI  := 0
	nPICM    := 0
	nPIPI    := 0
	oImposto := oDet[nX]
	cSitTrib := ""
	If Type("oImposto:_Imposto")<>"U"
		If Type("oImposto:_Imposto:_ICMS")<>"U"
			nLenSit := Len(aSitTrib)
			For nY := 1 To nLenSit
				nPrivate2 := nY
				If Type("oImposto:_Imposto:_ICMS:_ICMS"+aSitTrib[nPrivate2])<>"U"
					If Type("oImposto:_Imposto:_ICMS:_ICMS"+aSitTrib[nPrivate2]+":_VBC:TEXT")<>"U"
						nBaseICM := Val(&("oImposto:_Imposto:_ICMS:_ICMS"+aSitTrib[nY]+":_VBC:TEXT"))
						nValICM  := Val(&("oImposto:_Imposto:_ICMS:_ICMS"+aSitTrib[nY]+":_vICMS:TEXT"))
						nPICM    := Val(&("oImposto:_Imposto:_ICMS:_ICMS"+aSitTrib[nY]+":_PICMS:TEXT"))
					EndIf
					cSitTrib := &("oImposto:_Imposto:_ICMS:_ICMS"+aSitTrib[nY]+":_ORIG:TEXT")
					cSitTrib += &("oImposto:_Imposto:_ICMS:_ICMS"+aSitTrib[nY]+":_CST:TEXT")
				EndIf												
			Next nY			
		
			//Tratamento para o ICMS para optantes pelo Simples Nacional
			If Type("oEmitente:_CRT") <> "U" .And. oEmitente:_CRT:TEXT == "1"
				nLenSit := Len(aSitSN)
				For nY := 1 To nLenSit
					nPrivate2 := nY
					If Type("oImposto:_Imposto:_ICMS:_ICMSSN"+aSitSN[nPrivate2])<>"U"
						If Type("oImposto:_Imposto:_ICMS:_ICMSSN"+aSitSN[nPrivate2]+":_VBC:TEXT")<>"U"
							nBaseICM := Val(&("oImposto:_Imposto:_ICMS:_ICMSSN"+aSitSN[nY]+":_VBC:TEXT"))
							nValICM  := Val(&("oImposto:_Imposto:_ICMS:_ICMSSN"+aSitSN[nY]+":_vICMS:TEXT"))
							nPICM    := Val(&("oImposto:_Imposto:_ICMS:_ICMSSN"+aSitSN[nY]+":_PICMS:TEXT"))                   
						EndIf
						cSitTrib := &("oImposto:_Imposto:_ICMS:_ICMSSN"+aSitSN[nY]+":_CSOSN:TEXT")				
					EndIf
				Next nY	
			EndIf
		
		EndIf
		If Type("oImposto:_Imposto:_IPI")<>"U"
			If Type("oImposto:_Imposto:_IPI:_IPITrib:_vIPI:TEXT")<>"U"
				nValIPI := Val(oImposto:_Imposto:_IPI:_IPITrib:_vIPI:TEXT)
			EndIf
			If Type("oImposto:_Imposto:_IPI:_IPITrib:_pIPI:TEXT")<>"U"
				nPIPI   := Val(oImposto:_Imposto:_IPI:_IPITrib:_pIPI:TEXT)
			EndIf
		EndIf
	EndIf
	
	nMaxCod := MaxCod(oDet[nX]:_Prod:_cProd:TEXT, 80)
	
 nDecimais := IIF(MV_PAR04 == 1, TamSX3("D1_VUNIT")[2], TamSX3("D2_PRCVEN")[2])

	// Tratamento para quebrar os digitos dos valores
	aAux := {}
	AADD(aAux, AllTrim(TransForm(nQtd,TM(nQtd,TamSX3("D2_QUANT")[1],TamSX3("D2_QUANT")[2]))))
	AADD(aAux, AllTrim(TransForm(nVUnit,TM(nVUnit,TamSX3("D2_PRCVEN")[1],nDecimais))))
	AADD(aAux, AllTrim(TransForm(nVTotal,TM(nVTotal,TamSX3("D2_TOTAL")[1],TamSX3("D2_TOTAL")[2]))))
	AADD(aAux, AllTrim(TransForm(nBaseICM,TM(nBaseICM,TamSX3("D2_BASEICM")[1],TamSX3("D2_BASEICM")[2]))))
	AADD(aAux, AllTrim(TransForm(nValICM,TM(nValICM,TamSX3("D2_VALICM")[1],TamSX3("D2_VALICM")[2]))))
	AADD(aAux, AllTrim(TransForm(nValIPI,TM(nValIPI,TamSX3("D2_VALIPI")[1],TamSX3("D2_BASEIPI")[2]))))
	
	//**************************************************************************************** 
	// Inicio Roberto Fiuza 30/11/16 NSU - cliente  "MS0725" 
	//**************************************************************************************** 
	WX_NSU := ""
	dbSelectArea("SD2")
	dbSetOrder(3)
	MsSeek(xFilial("SD2")+SF2->F2_DOC+SF2->F2_SERIE+SF2->F2_CLIENTE+SF2->F2_LOJA)
	Do While !SD2->(Eof ()) .And. xFilial("SD2") == SD2->D2_FILIAL .And.;
		SF2->F2_DOC == SD2->D2_DOC . And. SF2->F2_SERIE == SD2->D2_SERIE .And.;
		SF2->F2_CLIENTE == SD2->D2_CLIENTE .And. SF2->F2_LOJA == SD2->D2_LOJA 
		
		dbSelectArea("SC6")
		dbSetOrder(1)
		MsSeek(xFilial("SC6") + SD2->D2_PEDIDO + SD2->D2_ITEMPV )
		IF SC6->C6_CLI = "MS0725"
		    IF ALLTRIM(C6_PRODUTO) = ALLTRIM(oDet[nX]:_Prod:_cProd:TEXT)
			   WX_NSU := ALLTRIM( STR(SC6->C6_X_NSU,10,0) )
			ENDIF   
		ENDIF
		
		dbSelectArea("SD2")
		SKIP
	ENDDO
	//**************************************************************************************** 
	// Inicio Roberto Fiuza 30/11/16 NSU - cliente  "MS0725" 
	//**************************************************************************************** 
	
	aadd(aItens,{;
		SubStr(oDet[nX]:_Prod:_cProd:TEXT,1,nMaxCod),;
		WX_NSU + " " + SubStr(NoChar(oDet[nX]:_Prod:_xProd:TEXT,lConverte),1,nMaxDes),;
		IIF(Type("oDet[nPrivate]:_Prod:_NCM")=="U","",oDet[nX]:_Prod:_NCM:TEXT),;
		cSitTrib,;
		oDet[nX]:_Prod:_CFOP:TEXT,;
		oDet[nX]:_Prod:_utrib:TEXT,;
		SubStr(aAux[1], 1, PosQuebrVal(aAux[1])),;
		SubStr(aAux[2], 1, PosQuebrVal(aAux[2])),;
		SubStr(aAux[3], 1, PosQuebrVal(aAux[3])),;
		SubStr(aAux[4], 1, PosQuebrVal(aAux[4])),;
		SubStr(aAux[5], 1, PosQuebrVal(aAux[5])),;
		SubStr(aAux[6], 1, PosQuebrVal(aAux[6])),;
		AllTrim(TransForm(nPICM,"@r 99.99%")),;
		AllTrim(TransForm(nPIPI,"@r 99.99%"));
	})
	
	cAuxItem := AllTrim(SubStr(oDet[nX]:_Prod:_cProd:TEXT,nMaxCod+1))
	cAux     := AllTrim(SubStr(NoChar(oDet[nX]:_Prod:_xProd:TEXT,lConverte),(nMaxDes+1)))
	aAux[1]  := SubStr(aAux[1], PosQuebrVal(aAux[1]) + 1)
	aAux[2]  := SubStr(aAux[2], PosQuebrVal(aAux[2]) + 1)
	aAux[3]  := SubStr(aAux[3], PosQuebrVal(aAux[3]) + 1)
	aAux[4]  := SubStr(aAux[4], PosQuebrVal(aAux[4]) + 1)
	aAux[5]  := SubStr(aAux[5], PosQuebrVal(aAux[5]) + 1)
	aAux[6]  := SubStr(aAux[6], PosQuebrVal(aAux[6]) + 1)

    lPontilhado := .F.	
	While !Empty(cAux) .Or. !Empty(cAuxItem) .Or. !Empty(aAux[1]) .Or. !Empty(aAux[2]) .Or. !Empty(aAux[3]) .Or. !Empty(aAux[4]) .Or. !Empty(aAux[5]) .Or. !Empty(aAux[6])
		nMaxCod := MaxCod(cAuxItem, 80)
		
		aadd(aItens,{;
			SubStr(cAuxItem,1,nMaxCod),;
			SubStr(cAux,1,nMaxDes),;
			"",;
			"",;
			"",;
			"",;
			SubStr(aAux[1], 1, PosQuebrVal(aAux[1])),;
			SubStr(aAux[2], 1, PosQuebrVal(aAux[2])),;
			SubStr(aAux[3], 1, PosQuebrVal(aAux[3])),;
			SubStr(aAux[4], 1, PosQuebrVal(aAux[4])),;
			SubStr(aAux[5], 1, PosQuebrVal(aAux[5])),;
			SubStr(aAux[6], 1, PosQuebrVal(aAux[6])),;
			"",;
			"";
		})
		
		// Popula as informações para as próximas linhas adicionais
		cAux        := SubStr(cAux,(nMaxDes+1))
		cAuxItem    := SubStr(cAuxItem,nMaxCod+1)
		aAux[1]     := SubStr(aAux[1], PosQuebrVal(aAux[1]) + 1)
		aAux[2]     := SubStr(aAux[2], PosQuebrVal(aAux[2]) + 1)
		aAux[3]     := SubStr(aAux[3], PosQuebrVal(aAux[3]) + 1)
		aAux[4]     := SubStr(aAux[4], PosQuebrVal(aAux[4]) + 1)
		aAux[5]     := SubStr(aAux[5], PosQuebrVal(aAux[5]) + 1)
		aAux[6]     := SubStr(aAux[6], PosQuebrVal(aAux[6]) + 1)
		lPontilhado := .T.	
	EndDo
	
	If (Type("oNf:_infnfe:_det[nPrivate]:_Infadprod:TEXT") <> "U" .Or. Type("oNf:_infnfe:_det:_Infadprod:TEXT") <> "U") .And. ( lImpAnfav .Or. lImpInfAd )
		cAux := stripTags(AllTrim(SubStr(oDet[nX]:_Infadprod:TEXT,1)), .T.)
		
		While !Empty(cAux)
			aadd(aItens,{;
				"",;
				SubStr(cAux,1,nMaxDes),;
				"",;
				"",;
				"",;
				"",;
				"",;
				"",;
				"",;
				"",;
				"",;
				"",;
				"",;
				"";
			})
			cAux := SubStr(cAux,(nMaxDes + 1))
	    	lPontilhado := .T.	
		EndDo
	EndIf
	If lPontilhado
		aadd(aItens,{;
			"-",;
			"-",;
			"-",;
			"-",;
			"-",;
			"-",;
			"-",;
			"-",;
			"-",;
			"-",;
			"-",;
			"-",;
			"-",;
			"-";
		})
	EndIf

Next nX
  

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Quadro ISSQN                                                            ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
aISSQN := {"","","",""}
If Type("oEmitente:_IM:TEXT")<>"U"
	aISSQN[1] := oEmitente:_IM:TEXT
EndIf
If Type("oTotal:_ISSQNtot")<>"U"
	aISSQN[2] := Transform(Val(oTotal:_ISSQNtot:_vServ:TEXT),"@ze 999,999,999.99")
	aISSQN[3] := Transform(Val(oTotal:_ISSQNtot:_vBC:TEXT),"@ze 999,999,999.99")
	aISSQN[4] := Transform(Val(oTotal:_ISSQNtot:_vISS:TEXT),"@ze 999,999,999.99")
EndIf

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Quadro de informacoes complementares                                    ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
aMensagem := {}
cAux := "******** PREVIEW da DANFE - SEM VALOR FISCAL ********"
If Type("oIdent:_tpAmb:TEXT")<>"U" .And. oIdent:_tpAmb:TEXT=="2"
	cAux := "DANFE emitida no ambiente de homologação - SEM VALOR FISCAL"
	While !Empty(cAux)
		aadd(aMensagem,SubStr(cAux,1,IIf(EspacoAt(cAux, MAXMENLIN) > 1, EspacoAt(cAux, MAXMENLIN) - 1, MAXMENLIN)))
		cAux := SubStr(cAux,IIf(EspacoAt(cAux, MAXMENLIN) > 1, EspacoAt(cAux, MAXMENLIN), MAXMENLIN) + 1)
	EndDo
EndIf

If Type("oNF:_InfNfe:_infAdic:_infAdFisco:TEXT")<>"U"
	cAux := oNF:_InfNfe:_infAdic:_infAdFisco:TEXT
	While !Empty(cAux)
  If (nPosSep := AT(";", cAux)) > 0 .And. nPosSep <= MAXMENLIN //Alpha Motion
   If !Empty(SubStr(cAux,1,nPosSep-1)) .Or. nPosSep > 1
    aadd(aMensagem,SubStr(cAux,1,nPosSep-1))
   EndIf 
   cAux := SubStr(cAux, nPosSep+1)
  ElseIf !Empty(SubStr(cAux,1,MAXMENLIN))
 		aadd(aMensagem,SubStr(cAux,1,IIf(EspacoAt(cAux, MAXMENLIN) > 1, EspacoAt(cAux, MAXMENLIN) - 1, MAXMENLIN)))
 		cAux := SubStr(cAux,IIf(EspacoAt(cAux, MAXMENLIN) > 1, EspacoAt(cAux, MAXMENLIN), MAXMENLIN) + 1)
 	EndIf	
//		aadd(aMensagem,SubStr(cAux,1,IIf(EspacoAt(cAux, MAXMENLIN) > 1, EspacoAt(cAux, MAXMENLIN) - 1, MAXMENLIN)))
//		cAux := SubStr(cAux,IIf(EspacoAt(cAux, MAXMENLIN) > 1, EspacoAt(cAux, MAXMENLIN), MAXMENLIN) + 1)
	EndDo
EndIf

If !Empty(cCodAutSef) .AND. oIdent:_tpEmis:TEXT<>"4"
	cAux := "Protocolo: "+cCodAutSef
	While !Empty(cAux)
		aadd(aMensagem,SubStr(cAux,1,IIf(EspacoAt(cAux, MAXMENLIN) > 1, EspacoAt(cAux, MAXMENLIN) - 1, MAXMENLIN)))
		cAux := SubStr(cAux,IIf(EspacoAt(cAux, MAXMENLIN) > 1, EspacoAt(cAux, MAXMENLIN), MAXMENLIN) + 1)
	EndDo
ElseIf !Empty(cCodAutSef) .AND. oIdent:_tpEmis:TEXT=="4" .AND. cModalidade $ "1"
	cAux := "Protocolo: "+cCodAutSef
	While !Empty(cAux)
		aadd(aMensagem,SubStr(cAux,1,IIf(EspacoAt(cAux, MAXMENLIN) > 1, EspacoAt(cAux, MAXMENLIN) - 1, MAXMENLIN)))
		cAux := SubStr(cAux,IIf(EspacoAt(cAux, MAXMENLIN) > 1, EspacoAt(cAux, MAXMENLIN), MAXMENLIN) + 1)
	EndDo
	cAux := "DANFE emitida anteriormente em contingência DPEC"
	While !Empty(cAux)
		aadd(aMensagem,SubStr(cAux,1,IIf(EspacoAt(cAux, MAXMENLIN) > 1, EspacoAt(cAux, MAXMENLIN) - 1, MAXMENLIN)))
		cAux := SubStr(cAux,IIf(EspacoAt(cAux, MAXMENLIN) > 1, EspacoAt(cAux, MAXMENLIN), MAXMENLIN) + 1)
	EndDo
EndIf

If !Empty(cCodAutDPEC) .And. oIdent:_tpEmis:TEXT=="4"
	cAux := "Número de Registro DPEC: "+cCodAutDPEC
	While !Empty(cAux)
		aadd(aMensagem,SubStr(cAux,1,IIf(EspacoAt(cAux, MAXMENLIN) > 1, EspacoAt(cAux, MAXMENLIN) - 1, MAXMENLIN)))
		cAux := SubStr(cAux,IIf(EspacoAt(cAux, MAXMENLIN) > 1, EspacoAt(cAux, MAXMENLIN), MAXMENLIN) + 1)
	EndDo
EndIf

If (Type("oIdent:_tpEmis:TEXT")<>"U" .And. !oIdent:_tpEmis:TEXT$"1,4")
	cAux := "DANFE emitida em contingência"
	While !Empty(cAux)
		aadd(aMensagem,SubStr(cAux,1,IIf(EspacoAt(cAux, MAXMENLIN) > 1, EspacoAt(cAux, MAXMENLIN) - 1, MAXMENLIN)))
		cAux := SubStr(cAux,IIf(EspacoAt(cAux, MAXMENLIN) > 1, EspacoAt(cAux, MAXMENLIN), MAXMENLIN) + 1)
	EndDo
ElseIf (!Empty(cModalidade) .And. !cModalidade $ "1,4,5") .And. Empty(cCodAutSef)
	cAux := "DANFE emitida em contingência devido a problemas técnicos - será necessária a substituição."
	While !Empty(cAux)
		aadd(aMensagem,SubStr(cAux,1,IIf(EspacoAt(cAux, MAXMENLIN) > 1, EspacoAt(cAux, MAXMENLIN) - 1, MAXMENLIN)))
		cAux := SubStr(cAux,IIf(EspacoAt(cAux, MAXMENLIN) > 1, EspacoAt(cAux, MAXMENLIN), MAXMENLIN) + 1)
	EndDo
ElseIf (!Empty(cModalidade) .And. cModalidade $ "5" .And. oIdent:_tpEmis:TEXT=="4")
	cAux := "DANFE impresso em contingência"
	While !Empty(cAux)
		aadd(aMensagem,SubStr(cAux,1,IIf(EspacoAt(cAux, MAXMENLIN) > 1, EspacoAt(cAux, MAXMENLIN) - 1, MAXMENLIN)))
		cAux := SubStr(cAux,IIf(EspacoAt(cAux, MAXMENLIN) > 1, EspacoAt(cAux, MAXMENLIN), MAXMENLIN) + 1)
	EndDo
	cAux := "DPEC regularmento recebido pela Receita Federal do Brasil."
	While !Empty(cAux)
		aadd(aMensagem,SubStr(cAux,1,IIf(EspacoAt(cAux, MAXMENLIN) > 1, EspacoAt(cAux, MAXMENLIN) - 1, MAXMENLIN)))
		cAux := SubStr(cAux,IIf(EspacoAt(cAux, MAXMENLIN) > 1, EspacoAt(cAux, MAXMENLIN), MAXMENLIN) + 1)
	EndDo
ElseIf (Type("oIdent:_tpEmis:TEXT")<>"U" .And. oIdent:_tpEmis:TEXT$"5")
	cAux := "DANFE emitida em contingência FS-DA"
	While !Empty(cAux)
		aadd(aMensagem,SubStr(cAux,1,IIf(EspacoAt(cAux, MAXMENLIN) > 1, EspacoAt(cAux, MAXMENLIN) - 1, MAXMENLIN)))
		cAux := SubStr(cAux,IIf(EspacoAt(cAux, MAXMENLIN) > 1, EspacoAt(cAux, MAXMENLIN), MAXMENLIN) + 1)
	EndDo
EndIf

If Type("oNF:_InfNfe:_infAdic:_infCpl:TEXT")<>"U"
	cAux := stripTags(oNF:_InfNfe:_infAdic:_InfCpl:TEXT, .T.)
	While !Empty(cAux)
		If (nPosSep := AT(";", cAux)) > 0 .And. nPosSep <= MAXMENLIN //Alpha Motion
   If !Empty(SubStr(cAux,1,nPosSep-1)) .Or. nPosSep > 1
    aadd(aMensagem,SubStr(cAux,1,nPosSep-1))
   EndIf 
   cAux := SubStr(cAux, nPosSep+1)
  ElseIf !Empty(SubStr(cAux,1,MAXMENLIN))
   aadd(aMensagem,SubStr(cAux,1,IIf(EspacoAt(cAux, MAXMENLIN) > 1, EspacoAt(cAux, MAXMENLIN) - 1, MAXMENLIN)))
		 cAux := SubStr(cAux,IIf(EspacoAt(cAux, MAXMENLIN) > 1, EspacoAt(cAux, MAXMENLIN), MAXMENLIN) + 1)
 	EndIf	
//		aadd(aMensagem,SubStr(cAux,1,IIf(EspacoAt(cAux, MAXMENLIN) > 1, EspacoAt(cAux, MAXMENLIN) - 1, MAXMENLIN)))
//		cAux := SubStr(cAux,IIf(EspacoAt(cAux, MAXMENLIN) > 1, EspacoAt(cAux, MAXMENLIN), MAXMENLIN) + 1)
	EndDo
EndIf
/*
dbSelectArea("SF1")
dbSetOrder(1)
If MsSeek(xFilial("SF1")+aNota[5]+aNota[4]+aNota[6]+aNota[7]) .And. SF1->(FieldPos("F1_FIMP"))<>0
	If SF1->F1_TIPO == "D"
		If Type("oNF:_InfNfe:_Total:_icmsTot:_VIPI:TEXT")<>"U"
			cAux := "Valor do Ipi : " + oNF:_InfNfe:_Total:_icmsTot:_VIPI:TEXT
			While !Empty(cAux)
				aadd(aMensagem,SubStr(cAux,1,IIf(EspacoAt(cAux, MAXMENLIN) > 1, EspacoAt(cAux, MAXMENLIN) - 1, MAXMENLIN)))
				cAux := SubStr(cAux,IIf(EspacoAt(cAux, MAXMENLIN) > 1, EspacoAt(cAux, MAXMENLIN), MAXMENLIN) + 1)
			EndDo
		EndIf      
	EndIf
	MsUnlock()
	DbSkip()
EndIf
*/                             
If lArt274SP .And. SuperGetMv("MV_ESTADO")$"SP"
	If Type("oNF:_INFNFE:_TOTAL:_ICMSTOT:_VBCST:TEXT") <> "U"
		If oNF:_INFNFE:_TOTAL:_ICMSTOT:_VBCST:TEXT <> "0"
			cAux := "Imposto recolhido por Substituição - Art 274 do RICMS"
			If oNF:_INFNFE:_DEST:_ENDERDEST:_UF:TEXT == "SP"
				cAux += ": "
				aLote := RastroNFOr(SD2->D2_DOC,SD2->D2_SERIE,SD2->D2_CLIENTE,SD2->D2_LOJA)
				For nX := 1 To Len(aLote)
					nBaseICM := aLote[nX][33]
					nValICM  := aLote[nX][38]
					cAux += Alltrim(aLote[nX][3]) + " - BCST: " + AllTrim(TransForm(nBaseICM,TM(nBaseICM,TamSX3("D1_BRICMS")[1],TamSX3("D1_BRICMS")[2]))) + " e ICMSST: " + ;
									AllTrim(TransForm(nValICM,TM(nValICM,TamSX3("D1_ICMSRET")[1],TamSX3("D1_ICMSRET")[2]))) + "/ " 
				Next nX                      
			Endif
			While !Empty(cAux)
				aadd(aMensagem,SubStr(cAux,1,IIf(EspacoAt(cAux, MAXMENLIN) > 1, EspacoAt(cAux, MAXMENLIN) - 1, MAXMENLIN)))
				cAux := SubStr(cAux,IIf(EspacoAt(cAux, MAXMENLIN) > 1, EspacoAt(cAux, MAXMENLIN), MAXMENLIN) + 1)
			EndDo
		Endif
	Endif
Endif     

If MV_PAR04 == 2
	//impressao do valor do desconto calculdo conforme decreto 43.080/02 RICMS-MG
	If !SF3->(Eof()) .And. SF2->F2_CLIENTE+SF2->F2_LOJA+SF2->F2_DOC+SF2->F2_SERIE == SF3->F3_CLIEFOR+SF3->F3_LOJA+SF3->F3_NFISCAL+SF3->F3_SERIE
	    If SF3->(FieldPos("F3_DS43080"))<>0 .And. SF3->F3_DS43080 > 0
			cAux := "Base de calc.reduzida conf.Art.43, Anexo IV, Parte 1, Item 3 do RICMS-MG. Valor da deducao ICMS R$ " 
			cAux += Alltrim(Transform(SF3->F3_DS43080,"@ze 9,999,999,999,999.99")) + " ref.reducao de base de calculo"  
			While !Empty(cAux)
				aadd(aMensagem,SubStr(cAux,1,IIf(EspacoAt(cAux, MAXMENLIN) > 1, EspacoAt(cAux, MAXMENLIN) - 1, MAXMENLIN)))
				cAux := SubStr(cAux,IIf(EspacoAt(cAux, MAXMENLIN) > 1, EspacoAt(cAux, MAXMENLIN), MAXMENLIN) + 1)
			EndDo                                                                                                                                                               
	    EndIf
	EndIf    
ElseIf MV_PAR04 == 1	
	//impressao do valor do desconto calculdo conforme decreto 43.080/02 RICMS-MG
	//Posiciono no SF3
	dbSelectArea("SF3")
	dbSetOrder(4)
	If MsSeek(xFilial("SF3")+SF1->F1_FORNECE+SF1->F1_LOJA+SF1->F1_DOC+SF1->F1_SERIE)	                                                                                                                                      		
		If SF3->(FieldPos("F3_DS43080"))<>0 .And. SF3->F3_DS43080 > 0
			cAux := "Base de calc.reduzida conf.Art.43, Anexo IV, Parte 1, Item 3 do RICMS-MG. Valor da deducao ICMS R$ " 
			cAux += Alltrim(Transform(SF3->F3_DS43080,"@ze 9,999,999,999,999.99")) + " ref.reducao de base de calculo"  
			While !Empty(cAux)
				aadd(aMensagem,SubStr(cAux,1,IIf(EspacoAt(cAux, MAXMENLIN) > 1, EspacoAt(cAux, MAXMENLIN) - 1, MAXMENLIN)))
				cAux := SubStr(cAux,IIf(EspacoAt(cAux, MAXMENLIN) > 1, EspacoAt(cAux, MAXMENLIN), MAXMENLIN) + 1)
			EndDo                                                                                                                                                               
	    EndIf                                                                                                                                  	
	EndIf
EndIF



For Nx := 1 to Len(aMensagem)
	NoChar(aMensagem[Nx],lConverte)
Next

If Type("oNF:_INFNFE:_IDE:_NFREF")<>"U"
	If Type("oNF:_INFNFE:_IDE:_NFREF") == "A"
		aInfNf := oNF:_INFNFE:_IDE:_NFREF
	Else
		aInfNf := {oNF:_INFNFE:_IDE:_NFREF}
	EndIf
	
	For nX := 1 to Len(aMensagem)
		If "ORIGINAL"$ Upper(aMensagem[nX])
			lNFori2 := .F.
		EndIf
	Next Nx
	
	cAux1 := ""
	cAux2 := ""
	For Nx := 1 to Len(aInfNf)
		If Type("aInfNf["+Str(nX)+"]:_REFNFE:TEXT")<>"U" .And. !AllTrim(aInfNf[nx]:_REFNFE:TEXT)$cAux1
			If !"CHAVE"$Upper(cAux1)
				cAux1 += "Chave de acesso da NF-E referenciada: "
			EndIf
			cAux1 += aInfNf[nx]:_REFNFE:TEXT+","
		ElseIf Type("aInfNf["+Str(nX)+"]:_REFNF:_NNF:TEXT")<>"U" .And. !AllTrim(aInfNf[nx]:_REFNF:_NNF:TEXT)$cAux2 .And. lNFori2
			If !"ORIGINAL"$Upper(cAux2)
				cAux2 += " Numero da nota original: "
			EndIf
			cAux2 += aInfNf[nx]:_REFNF:_NNF:TEXT+","
		EndIf
	Next
	
	cAux	:=	""
	If !Empty(cAux1)
		cAux1	:=	Left(cAux1,Len(cAux1)-1)
		cAux 	+= cAux1
	EndIf
	If !Empty(cAux2)
		cAux2	:=	Left(cAux2,Len(cAux2)-1)
		cAux 	+= 	Iif(!Empty(cAux),CRLF,"")+cAux2
	EndIf
	
	While !Empty(cAux)
		aadd(aMensagem,SubStr(cAux,1,IIf(EspacoAt(cAux, MAXMENLIN) > 1, EspacoAt(cAux, MAXMENLIN) - 1, MAXMENLIN)))
		cAux := SubStr(cAux,IIf(EspacoAt(cAux, MAXMENLIN) > 1, EspacoAt(cAux, MAXMENLIN), MAXMENLIN) + 1)
	EndDo

  	For Nx := 1 to Len(aMensagem)
   		NoChar(aMensagem[Nx],lConverte)
	Next

EndIf

//³Quadro "RESERVADO AO FISCO"                                             ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

aResFisco := {}
nBaseIcm  := 0

If GetNewPar("MV_BCREFIS",.F.) .And. SuperGetMv("MV_ESTADO")$"PR"
	If Val(&("oTotal:_ICMSTOT:_VBCST:TEXT")) <> 0
		cAux := "Substituição Tributária: Art. 471, II e §1º do RICMS/PR: "
   		nLenDet := Len(oDet)
   		For nX := 1 To nLenDet
	   		oImposto := oDet[nX]
	   		If Type("oImposto:_Imposto")<>"U"
		 		If Type("oImposto:_Imposto:_ICMS")<>"U"
		 			nLenSit := Len(aSitTrib)
		 			For nY := 1 To nLenSit
		 				nPrivate2 := nY
		 				If Type("oImposto:_Imposto:_ICMS:_ICMS"+aSitTrib[nPrivate2])<>"U"
		 					If Type("oImposto:_IMPOSTO:_ICMS:_ICMS"+aSitTrib[nPrivate2]+":_VBCST:TEXT")<>"U"
		 		   				nBaseIcm := Val(&("oImposto:_Imposto:_ICMS:_ICMS"+aSitTrib[nY]+":_VBCST:TEXT"))
		 						cAux += oDet[nX]:_PROD:_CPROD:TEXT + ": BCICMS-ST R$" + AllTrim(TransForm(nBaseICM,TM(nBaseICM,TamSX3("D2_BASEICM")[1],TamSX3("D2_BASEICM")[2]))) + " / "	
   		 	  				Endif
   		 	 			Endif
   					Next nY
   	   			Endif
   	 		Endif
   	   	Next nX
	Endif
	While !Empty(cAux)   
 		aadd(aResFisco,SubStr(cAux,1,60))
   		cAux := SubStr(cAux,IIf(EspacoAt(cAux, MAXMENLIN) > 1, 59, MAXMENLIN) +2)
	EndDo	
Endif
       
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Calculo do numero de folhas                                             ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ  
nFolhas	  := 1
nLenItens := Len(aItens) - MAXITEM // Todos os produtos/serviços excluindo a primeira página
nMsgCompl := Len(aMensagem) - MAXMSG // Todas as mensagens complementares excluindo a primeira página
lFlag     := .T.
While lFlag
	// Caso existam produtos/serviços e mensagens complementares a serem escritas
	If nLenItens > 0 .And. nMsgCompl > 0
		nFolhas++
		// Se estiver habilitado frente e verso e for uma página impar
		If MV_PAR05 == 1 .And. (nFolhas % 2) == 0
			nLenItens -= MAXITEMP3
		Else
			nLenItens -= MAXITEMP2
			nMsgCompl -= MAXMSG
		EndIf
	// Caso existam apenas mensagens complementares a serem escritas
	ElseIf nLenItens <= 0 .And. nMsgCompl > 0
		nFolhas++
		nMsgCompl := 0
	// Caso existam apenas produtos/serviços a serem escritos
	ElseIf nLenItens > 0 .And. nMsgCompl <= 0
		nFolhas++
		// Se estiver habilitado frente e verso e for uma página impar
		If MV_PAR05 == 1 .And. (nFolhas % 2) == 0
			nLenItens -= MAXITEMP3
		Else
			nLenItens -= MAXITEMP2F
		EndIf
	// Se não tiver mais nada a ser escrito fecha a contagem
	Else
		lFlag := .F.
	EndIf
EndDo

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Inicializacao do objeto grafico                                         ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
If oDanfe == Nil
	lPreview := .T.
	oDanfe 	:= FWMSPrinter():New("DANFE", IMP_SPOOL)
	oDanfe:SetPortrait()
	oDanfe:Setup()
EndIf

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Inicializacao da pagina do objeto grafico                               ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
oDanfe:StartPage()
nHPage := oDanfe:nHorzRes()
nHPage *= (300/PixelX)
nHPage -= HMARGEM
nVPage := oDanfe:nVertRes()
nVPage *= (300/PixelY)
nVPage -= VBOX

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Definicao do Box - Recibo de entrega                                    ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

oDanfe:Box(000,000,010,501)
oDanfe:Say(006, 002, "RECEBEMOS DE "+NoChar(oEmitente:_xNome:Text,lConverte)+" OS PRODUTOS CONSTANTES DA NOTA FISCAL INDICADA AO LADO", oFont07:oFont)
oDanfe:Box(009,000,037,101)
oDanfe:Say(017, 002, "DATA DE RECEBIMENTO", oFont07N:oFont)
oDanfe:Box(009,100,037,500)
oDanfe:Say(017, 102, "IDENTIFICAÇÃO E ASSINATURA DO RECEBEDOR", oFont07N:oFont)
oDanfe:Box(000,500,037,603)
oDanfe:Say(007, 542, "NF-e", oFont08N:oFont)
oDanfe:Say(017, 510, "N. "+StrZero(Val(oIdent:_NNf:Text),9), oFont08:oFont)
oDanfe:Say(027, 510, "SÉRIE "+oIdent:_Serie:Text, oFont08:oFont)


//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Quadro 1 IDENTIFICACAO DO EMITENTE                                      ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
oDanfe:Box(042,000,137,250)
oDanfe:Say(052,098, "Identificação do emitente",oFont12N:oFont)
nLinCalc	:=	065
cStrAux		:=	AllTrim(NoChar(oEmitente:_xNome:Text,lConverte))
nForTo		:=	Len(cStrAux)/28
nForTo		+=	Iif(nForTo>Round(nForTo,0),Round(nForTo,0)+1-nForTo,nForTo)
For nX := 1 To nForTo
	oDanfe:Say(nLinCalc,098,SubStr(cStrAux,Iif(nX==1,1,((nX-1)*28)+1),28), oFont12N:oFont )
	nLinCalc+=10
Next nX

cStrAux		:=	AllTrim(NoChar(oEmitente:_EnderEmit:_xLgr:Text,lConverte))+", "+AllTrim(oEmitente:_EnderEmit:_Nro:Text)
nForTo		:=	Len(cStrAux)/32
nForTo		+=	Iif(nForTo>Round(nForTo,0),Round(nForTo,0)+1-nForTo,nForTo)
For nX := 1 To nForTo
	oDanfe:Say(nLinCalc,098,SubStr(cStrAux,Iif(nX==1,1,((nX-1)*32)+1),32),oFont08N:oFont)
	nLinCalc+=10
Next nX

cTel := IIf(Type("oEmitente:_EnderEmit:_Fone")=="U","",oEmitente:_EnderEmit:_Fone:Text)
If Len(cTel) > 8
 cTel := "(" + SUBSTR(cTel, 1, 2) + ")" + SUBSTR(cTel, 3)
EndIf

If Type("oEmitente:_EnderEmit:_xCpl") <> "U"
	cStrAux		:=	"Complemento: "+AllTrim(NoChar(oEmitente:_EnderEmit:_xCpl:TEXT,lConverte))
	nForTo		:=	Len(cStrAux)/32
	nForTo		+=	Iif(nForTo>Round(nForTo,0),Round(nForTo,0)+1-nForTo,nForTo)
	For nX := 1 To nForTo
		oDanfe:Say(nLinCalc,098,SubStr(cStrAux,Iif(nX==1,1,((nX-1)*32)+1),32),oFont08N:oFont)
		nLinCalc+=10
	Next nX
	
	cStrAux		:=	AllTrim(oEmitente:_EnderEmit:_xBairro:Text)
	If Type("oEmitente:_EnderEmit:_Cep")<>"U"
		cStrAux		+=	" Cep:"+TransForm(oEmitente:_EnderEmit:_Cep:Text,"@r 99999-999")
	EndIf
	nForTo		:=	Len(cStrAux)/32
	nForTo		+=	Iif(nForTo>Round(nForTo,0),Round(nForTo,0)+1-nForTo,nForTo)
	For nX := 1 To nForTo
		oDanfe:Say(nLinCalc,098,SubStr(cStrAux,Iif(nX==1,1,((nX-1)*32)+1),32),oFont08N:oFont)
		nLinCalc+=10
	Next nX
 	oDanfe:Say(nLinCalc,098, oEmitente:_EnderEmit:_xMun:Text+"/"+oEmitente:_EnderEmit:_UF:Text,oFont08N:oFont)
 	nLinCalc+=10
 	oDanfe:Say(nLinCalc,098, "Fone: "+cTel,oFont08N:oFont)
Else
	oDanfe:Say(nLinCalc,098, NoChar(oEmitente:_EnderEmit:_xBairro:Text,lConverte)+" Cep:"+TransForm(IIF(Type("oEmitente:_EnderEmit:_Cep")=="U","",oEmitente:_EnderEmit:_Cep:Text),"@r 99999-999"),oFont08N:oFont)
	nLinCalc+=10
	oDanfe:Say(nLinCalc,098, oEmitente:_EnderEmit:_xMun:Text+"/"+oEmitente:_EnderEmit:_UF:Text,oFont08N:oFont)
 	nLinCalc+=10
	oDanfe:Say(nLinCalc,098, "Fone: "+cTel,oFont08N:oFont)
EndIf

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Quadro 2                                                                ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

oDanfe:Box(042,250,137,351)
oDanfe:Say(055,255, "DANFE",oFont18N:oFont)
oDanfe:Say(065,255, "DOCUMENTO AUXILIAR DA",oFont07:oFont)
oDanfe:Say(075,255, "NOTA FISCAL ELETRÔNICA",oFont07:oFont)
oDanfe:Say(085,255, "0-ENTRADA",oFont08:oFont)
oDanfe:Say(095,255, "1-SAÍDA"  ,oFont08:oFont)
oDanfe:Box(078,305,088,315)
oDanfe:Say(085,307, oIdent:_TpNf:Text,oFont08N:oFont)
oDanfe:Say(110,255,"N. "+StrZero(Val(oIdent:_NNf:Text),9),oFont10N:oFont)
oDanfe:Say(120,255,"SÉRIE "+oIdent:_Serie:Text,oFont10N:oFont)
oDanfe:Say(130,255,"FOLHA "+StrZero(nFolha,2)+"/"+StrZero(nFolhas,2),oFont10N:oFont)

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Preenchimento do Array de UF                                            ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
aadd(aUF,{"RO","11"})
aadd(aUF,{"AC","12"})
aadd(aUF,{"AM","13"})
aadd(aUF,{"RR","14"})
aadd(aUF,{"PA","15"})
aadd(aUF,{"AP","16"})
aadd(aUF,{"TO","17"})
aadd(aUF,{"MA","21"})
aadd(aUF,{"PI","22"})
aadd(aUF,{"CE","23"})
aadd(aUF,{"RN","24"})
aadd(aUF,{"PB","25"})
aadd(aUF,{"PE","26"})
aadd(aUF,{"AL","27"})
aadd(aUF,{"MG","31"})
aadd(aUF,{"ES","32"})
aadd(aUF,{"RJ","33"})
aadd(aUF,{"SP","35"})
aadd(aUF,{"PR","41"})
aadd(aUF,{"SC","42"})
aadd(aUF,{"RS","43"})
aadd(aUF,{"MS","50"})
aadd(aUF,{"MT","51"})
aadd(aUF,{"GO","52"})
aadd(aUF,{"DF","53"})
aadd(aUF,{"SE","28"})
aadd(aUF,{"BA","29"})
aadd(aUF,{"EX","99"})

nHPage := oDanfe:nHorzRes()
nHPage *= (300/PixelX)
nHPage -= HMARGEM
nVPage := oDanfe:nVertRes()
nVPage *= (300/PixelY)
nVPage -= VBOX

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Logotipo                                                                ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
If lMv_Logod
	cLogoD := GetSrvProfString("Startpath","") + "DANFE" + cEmpAnt + cFilAnt + ".BMP"
	If !File(cLogoD)
		cLogoD	:= GetSrvProfString("Startpath","") + "DANFE" + cEmpAnt + ".BMP"
		If !File(cLogoD)
			lMv_Logod := .F.
		EndIf
	EndIf
EndIf

If nfolha==1
	If lMv_Logod
		oDanfe:SayBitmap(043,001,cLogoD,094,093)
 Else
		oDanfe:SayBitmap(043,001,cLogo,094,093)
	EndIF
Endif

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Codigo de barra                                                         ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

oDanfe:Box(042,350,088,603)
oDanfe:Box(075,350,110,603)
oDanfe:Say(095,355,TransForm(SubStr(oNF:_InfNfe:_ID:Text,4),"@r 9999 9999 9999 9999 9999 9999 9999 9999 9999 9999 9999"),oFont12N:oFont)
oDanfe:Box(105,350,137,603)

If nFolha == 1
	oDanfe:Say(085,355,"CHAVE DE ACESSO DA NF-E",oFont12N:oFont)
	nFontSize := 28
	oDanfe:Code128C(072,370,SubStr(oNF:_InfNfe:_ID:Text,4), nFontSize )
EndIf

If !Empty(cCodAutDPEC) .And. (oNFe:_NFE:_INFNFE:_IDE:_TPEMIS:TEXT)$"4"
	cUF      := aUF[aScan(aUF,{|x| x[1] == oDPEC:_ENVDPEC:_INFDPEC:_RESNFE:_UF:Text})][02]
	cDataEmi := Substr(oNF:_InfNfe:_IDE:_DEMI:Text,9,2)
	cTPEmis  := "4"
	cValIcm  := StrZero(Val(StrTran(oDPEC:_ENVDPEC:_INFDPEC:_RESNFE:_VNF:TEXT,".","")),14)
	cICMSp   := iif(Val(oDPEC:_ENVDPEC:_INFDPEC:_RESNFE:_VICMS:TEXT)>0,"1","2")
	cICMSs   :=iif(Val(oDPEC:_ENVDPEC:_INFDPEC:_RESNFE:_VST:TEXT)>0,"1","2")
ElseIF (oNFe:_NFE:_INFNFE:_IDE:_TPEMIS:TEXT)$"25"
	cUF      := aUF[aScan(aUF,{|x| x[1] == oNFe:_NFE:_INFNFE:_DEST:_ENDERDEST:_UF:Text})][02]
	cDataEmi := Substr(oNFe:_NFE:_INFNFE:_IDE:_DEMI:Text,9,2)
	cTPEmis  := oNFe:_NFE:_INFNFE:_IDE:_TPEMIS:TEXT
	cValIcm  := StrZero(Val(StrTran(oNFe:_NFE:_INFNFE:_TOTAL:_ICMSTOT:_VNF:TEXT,".","")),14)
	cICMSp   := iif(Val(oNFe:_NFE:_INFNFE:_TOTAL:_ICMSTOT:_VICMS:TEXT)>0,"1","2")
	cICMSs   :=iif(Val(oNFe:_NFE:_INFNFE:_TOTAL:_ICMSTOT:_VST:TEXT)>0,"1","2")
EndIf
If !Empty(cUF) .And. !Empty(cDataEmi) .And. !Empty(cTPEmis) .And. !Empty(cValIcm) .And. !Empty(cICMSp) .And. !Empty(cICMSs)
	If Type("oNF:_InfNfe:_DEST:_CNPJ:Text")<>"U"
		cCNPJCPF := oNF:_InfNfe:_DEST:_CNPJ:Text
		If cUf == "99"
			cCNPJCPF := STRZERO(val(cCNPJCPF),14)
		EndIf
	ElseIf Type("oNF:_INFNFE:_DEST:_CPF:Text")<>"U"
		cCNPJCPF := oNF:_INFNFE:_DEST:_CPF:Text
		cCNPJCPF := STRZERO(val(cCNPJCPF),14)
	Else
		cCNPJCPF := ""
	EndIf
	cChaveCont += cUF+cTPEmis+cCNPJCPF+cValIcm+cICMSp+cICMSs+cDataEmi
	cChaveCont := cChaveCont+Modulo11(cChaveCont)
EndIf

If Empty(cCodAutDPEC)
	If Empty(cChaveCont)
//		oDanfe:Say(117,355,"Consulta de autenticidade no portal nacional da NF-e",oFont12:oFont)
//		oDanfe:Say(127,355,"www.nfe.fazenda.gov.br/portal ou no site da SEFAZ Autorizada",oFont12:oFont)
		oDanfe:Say(120,400,"*** PRE-DANFE ***",oFont18N:oFont)	
	Endif
Endif

If  !Empty(cCodAutDPEC)
//		oDanfe:Say(117,355,"Consulta de autenticidade no portal nacional da NF-e",oFont12:oFont)
//		oDanfe:Say(127,355,"www.nfe.fazenda.gov.br/portal ou no site da SEFAZ Autorizada",oFont12:oFont)
	oDanfe:Say(120,400,"*** PRE-DANFE ***",oFont18N:oFont)	
Endif

// inicio do segundo codigo de barras ref. a transmissao CONTIGENCIA OFF LINE
If !Empty(cChaveCont) .And. Empty(cCodAutDPEC) .And. !(Val(oNF:_INFNFE:_IDE:_SERIE:TEXT) >= 900)
	If nFolha == 1
		If !Empty(cChaveCont)
			nFontSize := 28
			oDanfe:Code128C(135,370,cChaveCont, nFontSize )
		EndIf
	Else
		If !Empty(cChaveCont)
			nFontSize := 28
			oDanfe:Code128C(112,370,cChaveCont, nFontSize )
		EndIf
	EndIf
EndIf

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Quadro 4                                                                ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

oDanfe:Box(139,000,162,603)
oDanfe:Box(139,000,162,350)
oDanfe:Say(148,002,"NATUREZA DA OPERAÇÃO",oFont08N:oFont)
oDanfe:Say(158,002,oIdent:_NATOP:TEXT,oFont08:oFont)


If !Empty(cCodAutDPEC)
	oDanfe:Say(148,350,"NÚMERO DE REGISTRO DPEC",oFont08N:oFont)
Endif

If Empty(cCodAutDPEC) .And. (((Val(oNF:_INFNFE:_IDE:_SERIE:TEXT) >= 900).And.(oNFe:_NFE:_INFNFE:_IDE:_TPEMIS:TEXT)$"23") .Or. (oNFe:_NFE:_INFNFE:_IDE:_TPEMIS:TEXT)$"1")
	oDanfe:Say(148,352,"PROTOCOLO DE AUTORIZAÇÃO DE USO",oFont08N:oFont)
Endif
If((oNFe:_NFE:_INFNFE:_IDE:_TPEMIS:TEXT)$"25")
	oDanfe:Say(148,352,"DADOS DA NF-E",oFont08N:oFont)
Endif
oDanfe:Say(158,354,IIF(!Empty(cCodAutDPEC),cCodAutDPEC+" "+AllTrim(IIF(!Empty(dDtReceb),ConvDate(DTOS(dDtReceb)),ConvDate(oNF:_InfNfe:_IDE:_DEMI:Text)))+" "+AllTrim(cDtHrRecCab),IIF(!Empty(cCodAutSef) .And. ((Val(oNF:_INFNFE:_IDE:_SERIE:TEXT) >= 900).And.(oNFe:_NFE:_INFNFE:_IDE:_TPEMIS:TEXT)$"23") .Or. (oNFe:_NFE:_INFNFE:_IDE:_TPEMIS:TEXT)$"1",cCodAutSef+" "+AllTrim(IIF(!Empty(dDtReceb),ConvDate(DTOS(dDtReceb)),ConvDate(oNF:_InfNfe:_IDE:_DEMI:Text)))+" "+AllTrim(cDtHrRecCab),TransForm(cChaveCont,"@r 9999 9999 9999 9999 9999 9999 9999 9999 9999"))),oFont08:oFont)
nFolha++


//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Quadro 5                                                                ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
oDanfe:Box(164,000,187,603)
oDanfe:Box(164,000,187,200)
oDanfe:Box(164,200,187,400)
oDanfe:Box(164,400,187,603)
oDanfe:Say(172,002,"INSCRIÇÃO ESTADUAL",oFont08N:oFont)
oDanfe:Say(180,002,IIf(Type("oEmitente:_IE:TEXT")<>"U",oEmitente:_IE:TEXT,""),oFont08:oFont)
oDanfe:Say(172,205,"INSC.ESTADUAL DO SUBST.TRIB.",oFont08N:oFont)
oDanfe:Say(180,205,IIf(Type("oEmitente:_IEST:TEXT")<>"U",oEmitente:_IEST:TEXT,""),oFont08:oFont)
oDanfe:Say(172,405,"CNPJ",oFont08N:oFont)
oDanfe:Say(180,405,TransForm(oEmitente:_CNPJ:TEXT,IIf(Len(oEmitente:_CNPJ:TEXT)<>14,"@r 999.999.999-99","@r 99.999.999/9999-99")),oFont08:oFont)
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Quadro destinatário/remetente                                           ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
Do Case
	Case Type("oDestino:_CNPJ")=="O"
		cAux := TransForm(oDestino:_CNPJ:TEXT,"@r 99.999.999/9999-99")
	Case Type("oDestino:_CPF")=="O"
		cAux := TransForm(oDestino:_CPF:TEXT,"@r 999.999.999-99")
	OtherWise
		cAux := Space(14)
EndCase


oDanfe:Say(195,002,"DESTINATARIO/REMETENTE",oFont08N:oFont)
oDanfe:Box(197,000,217,450)
oDanfe:Say(205,002, "NOME/RAZÃO SOCIAL",oFont08N:oFont)
oDanfe:Say(215,002,NoChar(oDestino:_XNome:TEXT,lConverte),oFont08:oFont)
oDanfe:Box(197,280,217,500)
oDanfe:Say(205,283,"CNPJ/CPF",oFont08N:oFont)
oDanfe:Say(215,283,cAux,oFont08:oFont)

oDanfe:Box(217,000,237,500)
oDanfe:Box(217,000,237,260)
oDanfe:Say(224,002,"ENDEREÇO",oFont08N:oFont)
oDanfe:Say(234,002,aDest[01],oFont08:oFont)
oDanfe:Box(217,230,237,380)
oDanfe:Say(224,232,"BAIRRO/DISTRITO",oFont08N:oFont)
oDanfe:Say(234,232,aDest[02],oFont08:oFont)
oDanfe:Box(217,380,237,500)
oDanfe:Say(224,382,"CEP",oFont08N:oFont)
oDanfe:Say(234,382,aDest[03],oFont08:oFont)

oDanfe:Box(236,000,257,500)
oDanfe:Box(236,000,257,180)
oDanfe:Say(245,002,"MUNICIPIO",oFont08N:oFont)
oDanfe:Say(255,002,aDest[05],oFont08:oFont)
oDanfe:Box(236,150,257,256)
oDanfe:Say(245,152,"FONE/FAX",oFont08N:oFont)
oDanfe:Say(255,152,aDest[06],oFont08:oFont)
oDanfe:Box(236,255,257,341)
oDanfe:Say(245,257,"UF",oFont08N:oFont)
oDanfe:Say(255,257,aDest[07],oFont08:oFont)
oDanfe:Box(236,340,257,500)
oDanfe:Say(245,342,"INSCRIÇÃO ESTADUAL",oFont08N:oFont)
oDanfe:Say(255,342,aDest[08],oFont08:oFont)


oDanfe:Box(197,502,217,603)
oDanfe:Say(205,504,"DATA DE EMISSÃO",oFont08N:oFont)
oDanfe:Say(215,504,ConvDate(oIdent:_DEmi:TEXT),oFont08:oFont)
oDanfe:Box(217,502,237,603)
oDanfe:Say(224,504,"DATA ENTRADA/SAÍDA",oFont08N:oFont)
oDanfe:Say(233,504,Iif( Empty(aDest[4]),"",ConvDate(aDest[4]) ),oFont08:oFont)
oDanfe:Box(236,502,257,603)
oDanfe:Say(243,503,"HORA ENTRADA/SAÍDA",oFont08N:oFont)
oDanfe:Say(252,503,aHrEnt[01],oFont08:oFont)
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Quadro fatura                                                           ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
aAux := {{{},{},{},{},{},{},{},{},{}}}
nY := 0
For nX := 1 To Len(aFaturas)
	nY++
	aadd(Atail(aAux)[nY],aFaturas[nX][1])
	nY++
	aadd(Atail(aAux)[nY],aFaturas[nX][2])
	nY++
	aadd(Atail(aAux)[nY],aFaturas[nX][3])
	nY++
	aadd(Atail(aAux)[nY],aFaturas[nX][4])
	nY++
	aadd(Atail(aAux)[nY],aFaturas[nX][5])
	nY++
	aadd(Atail(aAux)[nY],aFaturas[nX][6])
	nY++
	aadd(Atail(aAux)[nY],aFaturas[nX][7])
	nY++
	aadd(Atail(aAux)[nY],aFaturas[nX][8])
	nY++
	aadd(Atail(aAux)[nY],aFaturas[nX][9])
	If nY >= 9
		nY := 0
	EndIf
Next nX

oDanfe:Say(263,002,"FATURA",oFont08N:oFont)
oDanfe:Box(265,000,296,068)
oDanfe:Box(265,067,296,134)
oDanfe:Box(265,134,296,202)
oDanfe:Box(265,201,296,268)
oDanfe:Box(265,268,296,335)
oDanfe:Box(265,335,296,403)
oDanfe:Box(265,402,296,469)
oDanfe:Box(265,469,296,537)
oDanfe:Box(265,536,296,603)

nColuna := 002
If Len(aFaturas) >0
	For nY := 1 To 9
		oDanfe:Say(273,nColuna,aAux[1][nY][1],oFont08:oFont)
		oDanfe:Say(281,nColuna,aAux[1][nY][2],oFont08:oFont)
		oDanfe:Say(289,nColuna,aAux[1][nY][3],oFont08:oFont)
		nColuna:= nColuna+67
	Next nY
Endif
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Calculo do imposto                                                      ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
oDanfe:Say(305,002,"CALCULO DO IMPOSTO",oFont08N:oFont)
oDanfe:Box(307,000,330,121)
oDanfe:Say(316,002,"BASE DE CALCULO DO ICMS",oFont08N:oFont)
If cMVCODREG $ "3" 
	oDanfe:Say(326,002,aTotais[01],oFont08:oFont)
ElseIf lImpSimpN
	oDanfe:Say(326,002,aSimpNac[01],oFont08:oFont)	
Endif
oDanfe:Box(307,120,330,200)
oDanfe:Say(316,125,"VALOR DO ICMS",oFont08N:oFont)
If cMVCODREG $ "3" 
	oDanfe:Say(326,125,aTotais[02],oFont08:oFont)
ElseIf lImpSimpN
	oDanfe:Say(326,125,aSimpNac[02],oFont08:oFont)
Endif
oDanfe:Box(307,199,330,360)
oDanfe:Say(316,200,"BASE DE CALCULO DO ICMS SUBSTITUIÇÃO",oFont08N:oFont)
oDanfe:Say(326,202,aTotais[03],oFont08:oFont)
oDanfe:Box(307,360,330,490)
oDanfe:Say(316,363,"VALOR DO ICMS SUBSTITUIÇÃO",oFont08N:oFont)
oDanfe:Say(326,363,aTotais[04],oFont08:oFont)
oDanfe:Box(307,490,330,603)
oDanfe:Say(316,491,"VALOR TOTAL DOS PRODUTOS",oFont08N:oFont)
oDanfe:Say(327,491,aTotais[05],oFont08:oFont)


oDanfe:Box(330,000,353,110)
oDanfe:Say(339,002,"VALOR DO FRETE",oFont08N:oFont)
oDanfe:Say(349,002,aTotais[06],oFont08:oFont)
oDanfe:Box(330,100,353,190)
oDanfe:Say(339,102,"VALOR DO SEGURO",oFont08N:oFont)
oDanfe:Say(349,102,aTotais[07],oFont08:oFont)
oDanfe:Box(330,190,353,290)
oDanfe:Say(339,194,"DESCONTO",oFont08N:oFont)
oDanfe:Say(349,194,aTotais[08],oFont08:oFont)
oDanfe:Box(330,290,353,415)
oDanfe:Say(339,295,"OUTRAS DESPESAS ACESSÓRIAS",oFont08N:oFont)
oDanfe:Say(349,295,aTotais[09],oFont08:oFont)
oDanfe:Box(330,414,353,500)
oDanfe:Say(339,420,"VALOR DO IPI",oFont08N:oFont)
oDanfe:Say(349,420,aTotais[10],oFont08:oFont)
oDanfe:Box(330,500,353,603)
oDanfe:Say(339,506,"VALOR TOTAL DA NOTA",oFont08N:oFont)
oDanfe:Say(349,506,aTotais[11],oFont08:oFont)
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Transportador/Volumes transportados                                     ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
oDanfe:Say(361,002,"TRANSPORTADOR/VOLUMES TRANSPORTADOS",oFont08N:oFont)
oDanfe:Box(363,000,386,603)
oDanfe:Say(372,002,"RAZÃO SOCIAL",oFont08N:oFont)
oDanfe:Say(382,002,aTransp[01],oFont08:oFont)
oDanfe:Box(363,245,386,315)
oDanfe:Say(372,247,"FRETE POR CONTA",oFont08N:oFont)
If cModFrete =="0"
	oDanfe:Say(382,247,"0-EMITENTE",oFont08:oFont)
ElseIf cModFrete =="1"
	oDanfe:Say(382,247,"1-DEST/REM",oFont08:oFont)
ElseIf cModFrete =="2"
	oDanfe:Say(382,247,"2-TERCEIROS",oFont08:oFont)
ElseIf cModFrete =="9"
	oDanfe:Say(382,247,"9-SEM FRETE",oFont08:oFont)
Else
	oDanfe:Say(382,247,"",oFont08:oFont)
Endif
//oDanfe:Say(382,102,"0-EMITENTE/1-DESTINATARIO       [" + aTransp[02] + "]",oFont08:oFont)
oDanfe:Box(363,315,386,370)
oDanfe:Say(372,317,"CÓDIGO ANTT",oFont08N:oFont)
oDanfe:Say(382,319,aTransp[03],oFont08:oFont)
oDanfe:Box(363,370,386,490)
oDanfe:Say(372,375,"PLACA DO VEÍCULO",oFont08N:oFont)
oDanfe:Say(382,375,aTransp[04],oFont08:oFont)
oDanfe:Box(363,450,386,510)
oDanfe:Say(372,452,"UF",oFont08N:oFont)
oDanfe:Say(382,452,aTransp[05],oFont08:oFont)
oDanfe:Box(363,510,386,603)
oDanfe:Say(372,512,"CNPJ/CPF",oFont08N:oFont)
oDanfe:Say(382,512,aTransp[06],oFont08:oFont)

oDanfe:Box(385,000,409,603)
oDanfe:Box(385,000,409,241)
oDanfe:Say(393,002,"ENDEREÇO",oFont08N:oFont)
oDanfe:Say(404,002,aTransp[07],oFont08:oFont)
oDanfe:Box(385,240,409,341)
oDanfe:Say(393,242,"MUNICIPIO",oFont08N:oFont)
oDanfe:Say(404,242,aTransp[08],oFont08:oFont)
oDanfe:Box(385,340,409,440)
oDanfe:Say(393,342,"UF",oFont08N:oFont)
oDanfe:Say(404,342,aTransp[09],oFont08:oFont)
oDanfe:Box(385,440,409,603)
oDanfe:Say(393,442,"INSCRIÇÃO ESTADUAL",oFont08N:oFont)
oDanfe:Say(404,442,aTransp[10],oFont08:oFont)


oDanfe:Box(408,000,432,603)
oDanfe:Box(408,000,432,101)
oDanfe:Say(418,002,"QUANTIDADE",oFont08N:oFont)
oDanfe:Say(428,002,aTransp[11],oFont08:oFont)
oDanfe:Box(408,100,432,200)
oDanfe:Say(418,102,"ESPECIE",oFont08N:oFont)
oDanfe:Say(428,102,Iif(!Empty(aTransp[12]),aTransp[12],Iif(Len(aEspVol)>0,aEspVol[1][1],"")),oFont08:oFont)
//oDanfe:Say(428,102,aEspVol[1][1],oFont08:oFont)
oDanfe:Box(408,200,432,301)   

IF MV_PAR04 = 1                          
	dbSelectArea("SF1")
	dbSetOrder(1)
	MsSeek(xFilial("SF1")+aNota[5]+aNota[4]+aNota[6]+aNota[7])
ENDIF


oDanfe:Say(418,202,"MARCA",oFont08N:oFont)
IF MV_PAR04 = 1 .AND. SF1->F1_X_MARCA <> "    " // nf de entrada com marca preenchida  // Roberto Fiuza 04/08/14
   oDanfe:Say(428,202,SF1->F1_X_MARCA,oFont08:oFont)
ELSE
   oDanfe:Say(428,202,aTransp[13],oFont08:oFont)
ENDIF
oDanfe:Box(408,300,432,400)

oDanfe:Say(418,302,"NUMERAÇÃO",oFont08N:oFont)  
IF MV_PAR04 = 1 .AND. SF1->F1_X_NUMER <> "    " // nf de entrada com numeracao preenchida  // Roberto Fiuza 04/08/14
   oDanfe:Say(428,302,SF1->F1_X_NUMER,oFont08:oFont)
ELSE
	oDanfe:Say(428,302,aTransp[14],oFont08:oFont)
ENDIF

oDanfe:Box(408,400,432,501)


oDanfe:Say(418,402,"PESO BRUTO",oFont08N:oFont)
oDanfe:Say(428,402,Iif(!Empty(aTransp[15]),aTransp[15],Iif(Len(aEspVol)>0 .And. Val(aEspVol[1][3])>0,Transform(Val(aEspVol[1][3]),"@E 999999.9999"),"")),oFont08:oFont)
//oDanfe:Say(428,402,Iif (!Empty(aEspVol[1][3]),Transform(val(aEspVol[1][3]),"@E 999999.9999"),""),oFont08:oFont)
oDanfe:Box(408,500,432,603)
oDanfe:Say(418,502,"PESO LIQUIDO",oFont08N:oFont)
oDanfe:Say(428,502,Iif(!Empty(aTransp[16]),aTransp[16],Iif(Len(aEspVol)>0 .And. Val(aEspVol[1][2])>0,Transform(Val(aEspVol[1][2]),"@E 999999.9999"),"")),oFont08:oFont)
//oDanfe:Say(428,502,Iif (!Empty(aEspVol[1][2]),Transform(val(aEspVol[1][2]),"@E 999999.9999"),""),oFont08:oFont)

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Calculo do ISSQN                                                        ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

oDanfe:Say(686,000,"CALCULO DO ISSQN",oFont08N:oFont)
oDanfe:Box(688,000,711,151)
oDanfe:Say(696,002,"INSCRIÇÃO MUNICIPAL",oFont08N:oFont)
oDanfe:Say(706,002,aISSQN[1],oFont08:oFont)
oDanfe:Box(688,150,711,301)
oDanfe:Say(696,152,"VALOR TOTAL DOS SERVIÇOS",oFont08N:oFont)
oDanfe:Say(706,152,aISSQN[2],oFont08:oFont)
oDanfe:Box(688,300,711,451)
oDanfe:Say(696,302,"BASE DE CÁLCULO DO ISSQN",oFont08N:oFont)
oDanfe:Say(706,302,aISSQN[3],oFont08:oFont)
oDanfe:Box(688,450,711,603)
oDanfe:Say(696,452,"VALOR DO ISSQN",oFont08N:oFont)
oDanfe:Say(706,452,aISSQN[4],oFont08:oFont)

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Dados Adicionais                                                        ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
oDanfe:Say(719,000,"DADOS ADICIONAIS",oFont08N:oFont)
oDanfe:Box(721,000,865,351)
oDanfe:Say(729,002,"INFORMAÇÕES COMPLEMENTARES",oFont08N:oFont)

nLenMensagens:= Len(aMensagem)
nLin:= 741
nMensagem := 0
For nX := 1 To Min(nLenMensagens, MAXMSG)
	oDanfe:Say(nLin,002,aMensagem[nX],oFont08:oFont)
	nLin:= nLin+10
Next nX
nMensagem := nX

oDanfe:Box(721,350,865,603)
oDanfe:Say(729,352,"RESERVADO AO FISCO",oFont08N:oFont)

nLenMensagens:= Len(aResFisco)
nLin:= 741
For nX := 1 To Min(nLenMensagens, MAXMSG)
	oDanfe:Say(nLin,351,aResFisco[nX],oFont08:oFont)
	nLin:= nLin+10
Next

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Dados do produto ou servico                                             ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
aAux := {{{},{},{},{},{},{},{},{},{},{},{},{},{},{}}}
nY := 0
nLenItens := Len(aItens)

For nX :=1 To nLenItens
	nY++
	aadd(Atail(aAux)[nY],aItens[nX][01])
	nY++
	aadd(Atail(aAux)[nY],NoChar(aItens[nX][02],lConverte))
	nY++
	aadd(Atail(aAux)[nY],aItens[nX][03])
	nY++
	aadd(Atail(aAux)[nY],aItens[nX][04])
	nY++
	aadd(Atail(aAux)[nY],aItens[nX][05])
	nY++
	aadd(Atail(aAux)[nY],aItens[nX][06])
	nY++
	aadd(Atail(aAux)[nY],aItens[nX][07])
	nY++
	aadd(Atail(aAux)[nY],aItens[nX][08])
	nY++
	aadd(Atail(aAux)[nY],aItens[nX][09])
	nY++
	aadd(Atail(aAux)[nY],aItens[nX][10])
	nY++
	aadd(Atail(aAux)[nY],aItens[nX][11])
	nY++
	aadd(Atail(aAux)[nY],aItens[nX][12])
	nY++
	aadd(Atail(aAux)[nY],aItens[nX][13])
	nY++
	aadd(Atail(aAux)[nY],aItens[nX][14])
	If nY >= 14
		nY := 0
	EndIf
Next nX
For nX := 1 To nLenItens
	nY++
	aadd(Atail(aAux)[nY],"")
	nY++
	aadd(Atail(aAux)[nY],"")
	nY++
	aadd(Atail(aAux)[nY],"")
	nY++
	aadd(Atail(aAux)[nY],"")
	nY++
	aadd(Atail(aAux)[nY],"")
	nY++
	aadd(Atail(aAux)[nY],"")
	nY++
	aadd(Atail(aAux)[nY],"")
	nY++
	aadd(Atail(aAux)[nY],"")
	nY++
	aadd(Atail(aAux)[nY],"")
	nY++
	aadd(Atail(aAux)[nY],"")
	nY++
	aadd(Atail(aAux)[nY],"")
	nY++
	aadd(Atail(aAux)[nY],"")
	nY++
	aadd(Atail(aAux)[nY],"")
	nY++
	aadd(Atail(aAux)[nY],"")
	If nY >= 14
		nY := 0
	EndIf
	
Next nX

// Popula o array de cabeçalho das colunas de produtos/serviços.
aAuxCabec := {;
	"COD. PROD",;
	"DESCRIÇÃO DO PROD./SERV.",;
	"NCM/SH",;
	"CST",;
	"CFOP",;
	"UN",;
	"QUANT.",;
	"V.UNITARIO",;
	"V.TOTAL",;
	"BC.ICMS",;
	"V.ICMS",;
	"V.IPI",;
	"A.ICMS",;
	"A.IPI";
}

// Retorna o tamanho das colunas baseado em seu conteudo
aTamCol := RetTamCol(aAuxCabec, aAux, oDanfe, oFont08N:oFont, oFont08:oFont)

oDanfe:Say(440,002,"DADOS DO PRODUTO / SERVIÇO",oFont08N:oFont)
oDanfe:Box(442,000,678,603)
nAuxH := 0
oDanfe:Box(442, nAuxH, 678, nAuxH + aTamCol[1])
oDanfe:Say(450, nAuxH + 2, "COD. PROD",oFont08N:oFont)
nAuxH += aTamCol[1]
oDanfe:Box(442, nAuxH, 678, nAuxH + aTamCol[2])
oDanfe:Say(450, nAuxH + 2, "DESCRIÇÃO DO PROD./SERV.", oFont08N:oFont)
nAuxH += aTamCol[2]
oDanfe:Box(442, nAuxH, 678, nAuxH + aTamCol[3])
oDanfe:Say(450, nAuxH + 2, "NCM/SH", oFont08N:oFont)
nAuxH += aTamCol[3]
oDanfe:Box(442, nAuxH, 678, nAuxH + aTamCol[4])
oDanfe:Say(450, nAuxH + 2, "CST", oFont08N:oFont)
nAuxH += aTamCol[4]
oDanfe:Box(442, nAuxH, 678, nAuxH + aTamCol[5])
oDanfe:Say(450, nAuxH + 2, "CFOP", oFont08N:oFont)
nAuxH += aTamCol[5]
oDanfe:Box(442, nAuxH, 678, nAuxH + aTamCol[6])
oDanfe:Say(450, nAuxH + 2, "UN", oFont08N:oFont)
nAuxH += aTamCol[6]
oDanfe:Box(442, nAuxH, 678, nAuxH + aTamCol[7])
oDanfe:Say(450, nAuxH + 2, "QUANT.", oFont08N:oFont)
nAuxH += aTamCol[7]
oDanfe:Box(442, nAuxH, 678, nAuxH + aTamCol[8])
oDanfe:Say(450, nAuxH + 2, "V.UNITARIO", oFont08N:oFont)
nAuxH += aTamCol[8]
oDanfe:Box(442, nAuxH, 678, nAuxH + aTamCol[9])
oDanfe:Say(450, nAuxH + 2, "V.TOTAL", oFont08N:oFont)
nAuxH += aTamCol[9]
oDanfe:Box(442, nAuxH, 678, nAuxH + aTamCol[10])
oDanfe:Say(450, nAuxH + 2, "BC.ICMS", oFont08N:oFont)
nAuxH += aTamCol[10]
oDanfe:Box(442, nAuxH, 678, nAuxH + aTamCol[11])
oDanfe:Say(450, nAuxH + 2, "V.ICMS", oFont08N:oFont)
nAuxH += aTamCol[11]
oDanfe:Box(442, nAuxH, 678, nAuxH + aTamCol[12])
oDanfe:Say(450, nAuxH + 2, "V.IPI", oFont08N:oFont)
nAuxH += aTamCol[12]
oDanfe:Box(442, nAuxH, 678, nAuxH + aTamCol[13])
oDanfe:Say(450, nAuxH + 2, "A.ICMS", oFont08N:oFont)
nAuxH += aTamCol[13]
oDanfe:Box(442, nAuxH, 678, nAuxH + aTamCol[14])
oDanfe:Say(450, nAuxH + 2, "A.IPI", oFont08N:oFont)

If MV_PAR05=1 .And. nFolhas>1
	oDanfe:Say(875,497,"CONTINUA NO VERSO")
Endif

// INICIANDO INFORMAÇÕES PARA O CABEÇALHO DA PAGINA 2
nLinha	:= 460
nL	:= 0
lFlag	:= .T.

For nY := 1 To nLenItens
	nL++
	
	nLin:= 741
	nCont := 0
	
	If lflag
		If nL > nMaxItemP2
			oDanfe:EndPage()
			oDanfe:StartPage()
			If MV_PAR05 == 1
				nLinhavers := 42
			Else
				nLinhavers := 0
			EndIf		
			nLinha    	:=	181 + IIF(nFolha >=3 ,0, nLinhavers)
			
			oDanfe:Box(000+nLinhavers,000,095+nLinhavers,250)
			oDanfe:Say(010+nLinhavers,098, "Identificação do emitente",oFont12N:oFont)
			
			nLinCalc	:=	023 + nLinhavers
			cStrAux		:=	AllTrim(NoChar(oEmitente:_xNome:Text,lConverte))
			nForTo		:=	Len(cStrAux)/25
			nForTo		+=	Iif(nForTo>Round(nForTo,0),Round(nForTo,0)+1-nForTo,nForTo)
			For nX := 1 To nForTo
				oDanfe:Say(nLinCalc,098,SubStr(cStrAux,Iif(nX==1,1,((nX-1)*25)+1),25), oFont12N:oFont )
				nLinCalc+=10
			Next nX
			
			cStrAux		:=	AllTrim(NoChar(oEmitente:_EnderEmit:_xLgr:Text,lConverte))+", "+AllTrim(oEmitente:_EnderEmit:_Nro:Text)
			nForTo		:=	Len(cStrAux)/32
			nForTo		+=	Iif(nForTo>Round(nForTo,0),Round(nForTo,0)+1-nForTo,nForTo)
			For nX := 1 To nForTo
				oDanfe:Say(nLinCalc,098,SubStr(cStrAux,Iif(nX==1,1,((nX-1)*32)+1),32),oFont08N:oFont)
				nLinCalc+=10
			Next nX
			
			If Type("oEmitente:_EnderEmit:_xCpl") <> "U"
				cStrAux		:=	"Complemento: "+AllTrim(NoChar(oEmitente:_EnderEmit:_xCpl:TEXT,lConverte))
				nForTo		:=	Len(cStrAux)/32
				nForTo		+=	Iif(nForTo>Round(nForTo,0),Round(nForTo,0)+1-nForTo,nForTo)
				For nX := 1 To nForTo
					oDanfe:Say(nLinCalc,098,SubStr(cStrAux,Iif(nX==1,1,((nX-1)*32)+1),32),oFont08N:oFont)
					nLinCalc+=10
				Next nX
				
				cStrAux		:=	AllTrim(NoChar(oEmitente:_EnderEmit:_xBairro:Text,lConverte))
				If Type("oEmitente:_EnderEmit:_Cep")<>"U"
					cStrAux		+=	" Cep:"+TransForm(oEmitente:_EnderEmit:_Cep:Text,"@r 99999-999")
				EndIf
				nForTo		:=	Len(cStrAux)/32
				nForTo		+=	Iif(nForTo>Round(nForTo,0),Round(nForTo,0)+1-nForTo,nForTo)
				For nX := 1 To nForTo
					oDanfe:Say(nLinCalc,098,SubStr(cStrAux,Iif(nX==1,1,((nX-1)*32)+1),32),oFont08N:oFont)
					nLinCalc+=10
				Next nX
				oDanfe:Say(nLinCalc,098, oEmitente:_EnderEmit:_xMun:Text+"/"+oEmitente:_EnderEmit:_UF:Text,oFont08N:oFont)
				nLinCalc+=10
				oDanfe:Say(nLinCalc,098, "Fone: "+cTel,oFont08N:oFont)
			Else
				oDanfe:Say(nLinCalc,098, NoChar(oEmitente:_EnderEmit:_xBairro:Text,lConverte)+" Cep:"+TransForm(IIF(Type("oEmitente:_EnderEmit:_Cep")=="U","",oEmitente:_EnderEmit:_Cep:Text),"@r 99999-999"),oFont08N:oFont)
				nLinCalc+=10
				oDanfe:Say(nLinCalc,098, oEmitente:_EnderEmit:_xMun:Text+"/"+oEmitente:_EnderEmit:_UF:Text,oFont08N:oFont)
				nLinCalc+=10
				oDanfe:Say(nLinCalc,098, "Fone: "+cTel,oFont08N:oFont)
			EndIf
			
			oDanfe:Box(000+nLinhavers,248,095+nLinhavers,351)
			oDanfe:Say(013+nLinhavers,255, "DANFE",oFont18N:oFont)
			oDanfe:Say(023+nLinhavers,255, "DOCUMENTO AUXILIAR DA",oFont07:oFont)
			oDanfe:Say(033+nLinhavers,255, "NOTA FISCAL ELETRÔNICA",oFont07:oFont)
			oDanfe:Say(043+nLinhavers,255, "0-ENTRADA",oFont08:oFont)
			oDanfe:Say(053+nLinhavers,255, "1-SAÍDA"  ,oFont08:oFont)
			oDanfe:Box(037+nLinhavers,305,047+nLinhavers,315)
			oDanfe:Say(045+nLinhavers,307, oIdent:_TpNf:Text,oFont08N:oFont)
			oDanfe:Say(062+nLinhavers,255,"N. "+StrZero(Val(oIdent:_NNf:Text),9),oFont10N:oFont)
			oDanfe:Say(072+nLinhavers,255,"SÉRIE "+oIdent:_Serie:Text,oFont10N:oFont)
			oDanfe:Say(082+nLinhavers,255,"FOLHA "+StrZero(nFolha,2)+"/"+StrZero(nFolhas,2),oFont10N:oFont)
			
			oDanfe:Box(000+nLinhavers,350,095+nLinhavers,603)
			oDanfe:Box(000+nLinhavers,350,040+nLinhavers,603)
			oDanfe:Box(040+nLinhavers,350,062+nLinhavers,603)
			oDanfe:Box(063+nLinhavers,350,095+nLinhavers,603)
			oDanfe:Say(058+nLinhavers,355,TransForm(SubStr(oNF:_InfNfe:_ID:Text,4),"@r 9999 9999 9999 9999 9999 9999 9999 9999 9999 9999 9999"),oFont12N:oFont)
			
			oDanfe:Say(048+nLinhavers,355,"CHAVE DE ACESSO DA NF-E",oFont12N:oFont)
			nFontSize := 28
			oDanfe:Code128C(036+nLinhavers,370,SubStr(oNF:_InfNfe:_ID:Text,4), nFontSize )
			
			If lMv_Logod
				oDanfe:SayBitmap(000+nLinhavers,000,cLogoD,095,096)
			Else
				oDanfe:SayBitmap(000+nLinhavers,000,cLogo,095,096)
			EndIf
			
			If Empty(cChaveCont)
				oDanfe:Say(075+nLinhavers,355,"Consulta de autenticidade no portal nacional da NF-e",oFont12:oFont)
				oDanfe:Say(085+nLinhavers,355,"www.nfe.fazenda.gov.br/portal ou no site da SEFAZ Autorizada",oFont12:oFont)
			Endif
			
			If  !Empty(cCodAutDPEC)
				oDanfe:Say(075+nLinhavers,355,"Consulta de autenticidade no portal nacional da NF-e",oFont12:oFont)
				oDanfe:Say(085+nLinhavers,355,"www.nfe.fazenda.gov.br/portal ou no site da SEFAZ Autorizada",oFont12:oFont)
			Endif
			
			
			If nFolha == 1
				If !Empty(cCodAutDPEC)
					nFontSize := 28
					oDanfe:Code128C(093+nLinhavers,370,cCodAutDPEC, nFontSize )
				Endif
			Endif
			
			// inicio do segundo codigo de barras ref. a transmissao CONTIGENCIA OFF LINE
			If !Empty(cChaveCont) .And. Empty(cCodAutDPEC) .And. !(Val(oNF:_INFNFE:_IDE:_SERIE:TEXT) >= 900)
				If nFolha == 1
					If !Empty(cChaveCont)
						nFontSize := 28
						oDanfe:Code128C(093+nLinhavers,370,cChaveCont, nFontSize )
					EndIf
				Else
					If !Empty(cChaveCont)
						nFontSize := 28
						oDanfe:Code128C(093+nLinhavers,370,cChaveCont, nFontSize )
					EndIf
				EndIf
			EndIf
			
			oDanfe:Box(100+nLinhavers,000,123+nLinhavers,603)
			oDanfe:Box(100+nLinhavers,000,123+nLinhavers,300)
			oDanfe:Say(109+nLinhavers,002,"NATUREZA DA OPERAÇÃO",oFont08N:oFont)
			oDanfe:Say(119+nLinhavers,002,oIdent:_NATOP:TEXT,oFont08:oFont)
			If(!Empty(cCodAutDPEC))
				oDanfe:Say(109+nLinhavers,300,"NÚMERO DE REGISTRO DPEC",oFont08N:oFont)
			Endif
			If(((Val(oNF:_INFNFE:_IDE:_SERIE:TEXT) >= 900).And.(oNFe:_NFE:_INFNFE:_IDE:_TPEMIS:TEXT)$"2") .Or. (oNFe:_NFE:_INFNFE:_IDE:_TPEMIS:TEXT)$"1")
				oDanfe:Say(109+nLinhavers,302,"PROTOCOLO DE AUTORIZAÇÃO DE USO",oFont08N:oFont)
			Endif
			If((oNFe:_NFE:_INFNFE:_IDE:_TPEMIS:TEXT)$"25")
				oDanfe:Say(109+nLinhavers,300,"DADOS DA NF-E",oFont08N:oFont)
			Endif
			oDanfe:Say(119+nLinhavers,302,IIF(!Empty(cCodAutDPEC),cCodAutDPEC+" "+AllTrim(ConvDate(oNF:_InfNfe:_IDE:_DEMI:Text))+" "+AllTrim(cDtHrRecCab),IIF(!Empty(cCodAutSef) .And. ((Val(oNF:_INFNFE:_IDE:_SERIE:TEXT) >= 900).And.(oNFe:_NFE:_INFNFE:_IDE:_TPEMIS:TEXT)$"23") .Or. (oNFe:_NFE:_INFNFE:_IDE:_TPEMIS:TEXT)$"1",cCodAutSef+" "+AllTrim(ConvDate(oNF:_InfNfe:_IDE:_DEMI:Text))+" "+AllTrim(cDtHrRecCab),TransForm(cChaveCont,"@r 9999 9999 9999 9999 9999 9999 9999 9999 9999"))),oFont08:oFont)
							
			nFolha++
			
			oDanfe:Box(126+nLinhavers,000,153+nLinhavers,603)
			oDanfe:Box(126+nLinhavers,000,153+nLinhavers,200)
			oDanfe:Box(126+nLinhavers,200,153+nLinhavers,400)
			oDanfe:Box(126+nLinhavers,400,153+nLinhavers,603)
			oDanfe:Say(135+nLinhavers,002,"INSCRIÇÃO ESTADUAL",oFont08N:oFont)
			oDanfe:Say(143+nLinhavers,002,IIf(Type("oEmitente:_IE:TEXT")<>"U",oEmitente:_IE:TEXT,""),oFont08:oFont)
			oDanfe:Say(135+nLinhavers,205,"INSC.ESTADUAL DO SUBST.TRIB.",oFont08N:oFont)
			oDanfe:Say(143+nLinhavers,205,IIf(Type("oEmitente:_IEST:TEXT")<>"U",oEmitente:_IEST:TEXT,""),oFont08:oFont)
			oDanfe:Say(135+nLinhavers,405,"CNPJ",oFont08N:oFont)
			oDanfe:Say(143+nLinhavers,405,TransForm(oEmitente:_CNPJ:TEXT,IIf(Len(oEmitente:_CNPJ:TEXT)<>14,"@r 999.999.999-99","@r 99.999.999/9999-99")),oFont08:oFont)
			
			nLenMensagens:= Len(aMensagem)
			
			nColLim		:=	Iif(MV_PAR05==1,435,Iif(nMensagem <= nLenMensagens,680,865)) + nLinhavers 
			oDanfe:Say(161+nLinhavers,002,"DADOS DO PRODUTO / SERVIÇO",oFont08N:oFont)
			oDanfe:Box(163+nLinhavers,000,nColLim,603)
			
			nAuxH := 0
			oDanfe:Box(163+nLinhavers, nAuxH, nColLim, nAuxH + aTamCol[1])
			oDanfe:Say(171+nLinhavers, nAuxH + 2, "COD. PROD",oFont08N:oFont)
			nAuxH += aTamCol[1]
			oDanfe:Box(163+nLinhavers, nAuxH, nColLim, nAuxH + aTamCol[2])
			oDanfe:Say(171+nLinhavers, nAuxH + 2, "DESCRIÇÃO DO PROD./SERV.", oFont08N:oFont)
			nAuxH += aTamCol[2]
			oDanfe:Box(163+nLinhavers, nAuxH, nColLim, nAuxH + aTamCol[3])
			oDanfe:Say(171+nLinhavers, nAuxH + 2, "NCM/SH", oFont08N:oFont)
			nAuxH += aTamCol[3]
			oDanfe:Box(163+nLinhavers, nAuxH, nColLim, nAuxH + aTamCol[4])
			oDanfe:Say(171+nLinhavers, nAuxH + 2, "CST", oFont08N:oFont)
			nAuxH += aTamCol[4]
			oDanfe:Box(163+nLinhavers, nAuxH, nColLim, nAuxH + aTamCol[5])
			oDanfe:Say(171+nLinhavers, nAuxH + 2, "CFOP", oFont08N:oFont)
			nAuxH += aTamCol[5]
			oDanfe:Box(163+nLinhavers, nAuxH, nColLim, nAuxH + aTamCol[6])
			oDanfe:Say(171+nLinhavers, nAuxH + 2, "UN", oFont08N:oFont)
			nAuxH += aTamCol[6]
			oDanfe:Box(163+nLinhavers, nAuxH, nColLim, nAuxH + aTamCol[7])
			oDanfe:Say(171+nLinhavers, nAuxH + 2, "QUANT.", oFont08N:oFont)
			nAuxH += aTamCol[7]
			oDanfe:Box(163+nLinhavers, nAuxH, nColLim, nAuxH + aTamCol[8])
			oDanfe:Say(171+nLinhavers, nAuxH + 2, "V.UNITARIO", oFont08N:oFont)
			nAuxH += aTamCol[8]
			oDanfe:Box(163+nLinhavers, nAuxH, nColLim, nAuxH + aTamCol[9])
			oDanfe:Say(171+nLinhavers, nAuxH + 2, "V.TOTAL", oFont08N:oFont)
			nAuxH += aTamCol[9]
			oDanfe:Box(163+nLinhavers, nAuxH, nColLim, nAuxH + aTamCol[10])
			oDanfe:Say(171+nLinhavers, nAuxH + 2, "BC.ICMS", oFont08N:oFont)
			nAuxH += aTamCol[10]
			oDanfe:Box(163+nLinhavers, nAuxH, nColLim, nAuxH + aTamCol[11])
			oDanfe:Say(171+nLinhavers, nAuxH + 2, "V.ICMS", oFont08N:oFont)
			nAuxH += aTamCol[11]
			oDanfe:Box(163+nLinhavers, nAuxH, nColLim, nAuxH + aTamCol[12])
			oDanfe:Say(171+nLinhavers, nAuxH + 2, "V.IPI", oFont08N:oFont)
			nAuxH += aTamCol[12]
			oDanfe:Box(163+nLinhavers, nAuxH, nColLim, nAuxH + aTamCol[13])
			oDanfe:Say(171+nLinhavers, nAuxH + 2, "A.ICMS", oFont08N:oFont)
			nAuxH += aTamCol[13]
			oDanfe:Box(163+nLinhavers, nAuxH, nColLim, nAuxH + aTamCol[14])
			oDanfe:Say(171+nLinhavers, nAuxH + 2, "A.IPI", oFont08N:oFont)
			
			// FINALIZANDO INFORMAÇÕES PARA O CABEÇALHO DA PAGINA 2
			nL	:= 1
			lFlag	:= .F.                                         		
			
			//Verifico se ainda existem Dados Adicionais a serem impressos
			IF MV_PAR05 <> 1 .And. nMensagem <= nLenMensagens
				//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
				//³Dados Adicionais                                                        ³
				//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
				oDanfe:Say(719+nLinhavers,000,"DADOS ADICIONAIS",oFont08N:oFont)
				oDanfe:Box(721+nLinhavers,000,865+nLinhavers,351)
				oDanfe:Say(729+nLinhavers,002,"INFORMAÇÕES COMPLEMENTARES",oFont08N:oFont)				
				
				nLin:= 741
				nLenMensagens:= Len(aMensagem)
				--nMensagem
				For nX := 1 To Min(nLenMensagens - nMensagem, MAXMSG)
					oDanfe:Say(nLin,002,aMensagem[nMensagem+nX],oFont08:oFont)
					nLin:= nLin+10
				Next nX
				nMensagem := nMensagem+nX
				
				oDanfe:Box(721+nLinhavers,350,865+nLinhavers,603)
				oDanfe:Say(729+nLinhavers,352,"RESERVADO AO FISCO",oFont08N:oFont)
				
				// Seta o máximo de itens para o MAXITEMP2
				nMaxItemP2 := MAXITEMP2
			Else
				// Seta o máximo de itens para o MAXITEMP2F
				nMaxItemP2 := MAXITEMP2F
			EndIF
		Endif		
	Endif
	
	// INICIANDO INFORMAÇÕES PARA O CABEÇALHO DA PAGINA 3 E DIANTE	
	If	nL > Iif( (nfolha-1)%2==0 .And. MV_PAR05==1,MAXITEMP3,nMaxItemP2)
		oDanfe:EndPage()
		oDanfe:StartPage()
		nLenMensagens:= Len(aMensagem)							
		nColLim		:=	Iif(!(nfolha-1)%2==0 .And. MV_PAR05==1,435,Iif(nMensagem <= nLenMensagens,680,865))
		lFimpar		:=  ((nfolha-1)%2==0)
		nLinha    	:=	181      
		If nfolha >= 3
			nLinhavers := 0
		EndIf
		oDanfe:Box(000,000,095,250)
		oDanfe:Say(010,098, "Identificação do emitente",oFont12N:oFont)
		nLinCalc	:=	023
		cStrAux		:=	AllTrim(NoChar(oEmitente:_xNome:Text,lConverte))
		nForTo		:=	Len(cStrAux)/25
		nForTo		+=	Iif(nForTo>Round(nForTo,0),Round(nForTo,0)+1-nForTo,nForTo)
		For nX := 1 To nForTo
			oDanfe:Say(nLinCalc,098,SubStr(cStrAux,Iif(nX==1,1,((nX-1)*25)+1),25), oFont12N:oFont )
			nLinCalc+=10
		Next nX
		
		cStrAux		:=	AllTrim(NoChar(oEmitente:_EnderEmit:_xLgr:Text,lConverte))+", "+AllTrim(oEmitente:_EnderEmit:_Nro:Text)
		nForTo		:=	Len(cStrAux)/32
		nForTo		+=	Iif(nForTo>Round(nForTo,0),Round(nForTo,0)+1-nForTo,nForTo)
		For nX := 1 To nForTo
			oDanfe:Say(nLinCalc,098,SubStr(cStrAux,Iif(nX==1,1,((nX-1)*32)+1),32),oFont08N:oFont)
			nLinCalc+=10
		Next nX
		
		If Type("oEmitente:_EnderEmit:_xCpl") <> "U"
			cStrAux		:=	"Complemento: "+AllTrim(NoChar(oEmitente:_EnderEmit:_xCpl:TEXT,lConverte))
			nForTo		:=	Len(cStrAux)/32
			nForTo		+=	Iif(nForTo>Round(nForTo,0),Round(nForTo,0)+1-nForTo,nForTo)
			For nX := 1 To nForTo
				oDanfe:Say(nLinCalc,098,SubStr(cStrAux,Iif(nX==1,1,((nX-1)*32)+1),32),oFont08N:oFont)
				nLinCalc+=10
			Next nX
			
			cStrAux		:=	AllTrim(NoChar(oEmitente:_EnderEmit:_xBairro:Text,lConverte))
			If Type("oEmitente:_EnderEmit:_Cep")<>"U"
				cStrAux		+=	" Cep:"+TransForm(oEmitente:_EnderEmit:_Cep:Text,"@r 99999-999")
			EndIf
			nForTo		:=	Len(cStrAux)/32
			nForTo		+=	Iif(nForTo>Round(nForTo,0),Round(nForTo,0)+1-nForTo,nForTo)
			For nX := 1 To nForTo
				oDanfe:Say(nLinCalc,098,SubStr(cStrAux,Iif(nX==1,1,((nX-1)*32)+1),32),oFont08N:oFont)
				nLinCalc+=10
			Next nX
			oDanfe:Say(nLinCalc,098, oEmitente:_EnderEmit:_xMun:Text+"/"+oEmitente:_EnderEmit:_UF:Text,oFont08N:oFont)
			nLinCalc+=10
			oDanfe:Say(nLinCalc,098, "Fone: "+cTel,oFont08N:oFont)
		Else
			oDanfe:Say(nLinCalc,098, NoChar(oEmitente:_EnderEmit:_xBairro:Text,lConverte)+" Cep:"+TransForm(IIF(Type("oEmitente:_EnderEmit:_Cep")=="U","",oEmitente:_EnderEmit:_Cep:Text),"@r 99999-999"),oFont08N:oFont)
			nLinCalc+=10
			oDanfe:Say(nLinCalc,098, oEmitente:_EnderEmit:_xMun:Text+"/"+oEmitente:_EnderEmit:_UF:Text,oFont08N:oFont)
			nLinCalc+=10
			oDanfe:Say(nLinCalc,098, "Fone: "+cTel,oFont08N:oFont)
		EndIf
		
		oDanfe:Box(000,248,095,351)
		oDanfe:Say(013,255, "DANFE",oFont18N:oFont)
		oDanfe:Say(023,255, "DOCUMENTO AUXILIAR DA",oFont07:oFont)
		oDanfe:Say(033,255, "NOTA FISCAL ELETRÔNICA",oFont07:oFont)
		oDanfe:Say(043,255, "0-ENTRADA",oFont08:oFont)
		oDanfe:Say(053,255, "1-SAÍDA"  ,oFont08:oFont)
		oDanfe:Box(037,305,047,315)
		oDanfe:Say(045,307, oIdent:_TpNf:Text,oFont08N:oFont)
		oDanfe:Say(062,255,"N. "+StrZero(Val(oIdent:_NNf:Text),9),oFont10N:oFont)
		oDanfe:Say(072,255,"SÉRIE "+oIdent:_Serie:Text,oFont10N:oFont)
		oDanfe:Say(082,255,"FOLHA "+StrZero(nFolha,2)+"/"+StrZero(nFolhas,2),oFont10N:oFont)
		
		oDanfe:Box(000,350,095,603)
		oDanfe:Box(000,350,040,603)
		oDanfe:Box(040,350,062,603)
		oDanfe:Box(063,350,095,603)
		oDanfe:Say(058,355,TransForm(SubStr(oNF:_InfNfe:_ID:Text,4),"@r 9999 9999 9999 9999 9999 9999 9999 9999 9999 9999 9999"),oFont12N:oFont)
		
		oDanfe:Say(048,355,"CHAVE DE ACESSO DA NF-E",oFont12N:oFont)
		nFontSize := 28
		oDanfe:Code128C(036,370,SubStr(oNF:_InfNfe:_ID:Text,4), nFontSize )
		
		If lMv_Logod
			oDanfe:SayBitmap(001,001,cLogoD,094,093)
		Else
			oDanfe:SayBitmap(001,001,cLogo,094,093)
		EndIf
		
		If Empty(cChaveCont)
			oDanfe:Say(075,355,"Consulta de autenticidade no portal nacional da NF-e",oFont12:oFont)
			oDanfe:Say(085,355,"www.nfe.fazenda.gov.br/portal ou no site da SEFAZ Autorizada",oFont12:oFont)
		Endif
		
		If  !Empty(cCodAutDPEC)
			oDanfe:Say(075,355,"Consulta de autenticidade no portal nacional da NF-e",oFont12:oFont)
			oDanfe:Say(085,355,"www.nfe.fazenda.gov.br/portal ou no site da SEFAZ Autorizada",oFont12:oFont)
		Endif
		
		
		If nFolha == 1
			If !Empty(cCodAutDPEC)
				nFontSize := 28
				oDanfe:Code128C(093,370,cCodAutDPEC, nFontSize )
			Endif
		Endif
		
		// inicio do segundo codigo de barras ref. a transmissao CONTIGENCIA OFF LINE
		If !Empty(cChaveCont) .And. Empty(cCodAutDPEC) .And. !(Val(oNF:_INFNFE:_IDE:_SERIE:TEXT) >= 900)
			If nFolha == 1
				If !Empty(cChaveCont)
					nFontSize := 28
					oDanfe:Code128C(093,370,cChaveCont, nFontSize )
				EndIf
			Else
				If !Empty(cChaveCont)
					nFontSize := 28
					oDanfe:Code128C(093,370,cChaveCont, nFontSize )
				EndIf
			EndIf
		EndIf
		
		oDanfe:Box(100,000,123,603)
		oDanfe:Box(100,000,123,300)
		oDanfe:Say(109,002,"NATUREZA DA OPERAÇÃO",oFont08N:oFont)
		oDanfe:Say(119,002,oIdent:_NATOP:TEXT,oFont08:oFont)
		If(!Empty(cCodAutDPEC))
			oDanfe:Say(109,300,"NÚMERO DE REGISTRO DPEC",oFont08N:oFont)
		Endif
		If(((Val(oNF:_INFNFE:_IDE:_SERIE:TEXT) >= 900).And.(oNFe:_NFE:_INFNFE:_IDE:_TPEMIS:TEXT)$"2") .Or. (oNFe:_NFE:_INFNFE:_IDE:_TPEMIS:TEXT)$"1")
			oDanfe:Say(109,302,"PROTOCOLO DE AUTORIZAÇÃO DE USO",oFont08N:oFont)
		Endif
		If((oNFe:_NFE:_INFNFE:_IDE:_TPEMIS:TEXT)$"25")
			oDanfe:Say(109,300,"DADOS DA NF-E",oFont08N:oFont)
		Endif
		oDanfe:Say(119,302,IIF(!Empty(cCodAutDPEC),cCodAutDPEC+" "+AllTrim(ConvDate(oNF:_InfNfe:_IDE:_DEMI:Text))+" "+AllTrim(cDtHrRecCab),IIF(!Empty(cCodAutSef) .And. ((Val(oNF:_INFNFE:_IDE:_SERIE:TEXT) >= 900).And.(oNFe:_NFE:_INFNFE:_IDE:_TPEMIS:TEXT)$"23") .Or. (oNFe:_NFE:_INFNFE:_IDE:_TPEMIS:TEXT)$"1",cCodAutSef+" "+AllTrim(ConvDate(oNF:_InfNfe:_IDE:_DEMI:Text))+" "+AllTrim(cDtHrRecCab),TransForm(cChaveCont,"@r 9999 9999 9999 9999 9999 9999 9999 9999 9999"))),oFont08:oFont)
		nFolha++
		
		oDanfe:Box(126,000,153,603)
		oDanfe:Box(126,000,153,200)
		oDanfe:Box(126,200,153,400)
		oDanfe:Box(126,400,153,603)
		oDanfe:Say(135,002,"INSCRIÇÃO ESTADUAL",oFont08N:oFont)
		oDanfe:Say(143,002,IIf(Type("oEmitente:_IE:TEXT")<>"U",oEmitente:_IE:TEXT,""),oFont08:oFont)
		oDanfe:Say(135,205,"INSC.ESTADUAL DO SUBST.TRIB.",oFont08N:oFont)
		oDanfe:Say(143,205,IIf(Type("oEmitente:_IEST:TEXT")<>"U",oEmitente:_IEST:TEXT,""),oFont08:oFont)
		oDanfe:Say(135,405,"CNPJ",oFont08N:oFont)
		oDanfe:Say(143,405,TransForm(oEmitente:_CNPJ:TEXT,IIf(Len(oEmitente:_CNPJ:TEXT)<>14,"@r 999.999.999-99","@r 99.999.999/9999-99")),oFont08:oFont)
		
		oDanfe:Say(161,002,"DADOS DO PRODUTO / SERVIÇO",oFont08N:oFont)
		oDanfe:Box(163,000,nColLim,603)
		
		nAuxH := 0
		oDanfe:Box(163, nAuxH, nColLim, nAuxH + aTamCol[1])
		oDanfe:Say(171, nAuxH + 2, "COD. PROD",oFont08N:oFont)
		nAuxH += aTamCol[1]
		oDanfe:Box(163, nAuxH, nColLim, nAuxH + aTamCol[2])
		oDanfe:Say(171, nAuxH + 2, "DESCRIÇÃO DO PROD./SERV.", oFont08N:oFont)
		nAuxH += aTamCol[2]
		oDanfe:Box(163, nAuxH, nColLim, nAuxH + aTamCol[3])
		oDanfe:Say(171, nAuxH + 2, "NCM/SH", oFont08N:oFont)
		nAuxH += aTamCol[3]
		oDanfe:Box(163, nAuxH, nColLim, nAuxH + aTamCol[4])
		oDanfe:Say(171, nAuxH + 2, "CST", oFont08N:oFont)
		nAuxH += aTamCol[4]
		oDanfe:Box(163, nAuxH, nColLim, nAuxH + aTamCol[5])
		oDanfe:Say(171, nAuxH + 2, "CFOP", oFont08N:oFont)
		nAuxH += aTamCol[5]
		oDanfe:Box(163, nAuxH, nColLim, nAuxH + aTamCol[6])
		oDanfe:Say(171, nAuxH + 2, "UN", oFont08N:oFont)
		nAuxH += aTamCol[6]
		oDanfe:Box(163, nAuxH, nColLim, nAuxH + aTamCol[7])
		oDanfe:Say(171, nAuxH + 2, "QUANT.", oFont08N:oFont)
		nAuxH += aTamCol[7]
		oDanfe:Box(163, nAuxH, nColLim, nAuxH + aTamCol[8])
		oDanfe:Say(171, nAuxH + 2, "V.UNITARIO", oFont08N:oFont)
		nAuxH += aTamCol[8]
		oDanfe:Box(163, nAuxH, nColLim, nAuxH + aTamCol[9])
		oDanfe:Say(171, nAuxH + 2, "V.TOTAL", oFont08N:oFont)
		nAuxH += aTamCol[9]
		oDanfe:Box(163, nAuxH, nColLim, nAuxH + aTamCol[10])
		oDanfe:Say(171, nAuxH + 2, "BC.ICMS", oFont08N:oFont)
		nAuxH += aTamCol[10]
		oDanfe:Box(163, nAuxH, nColLim, nAuxH + aTamCol[11])
		oDanfe:Say(171, nAuxH + 2, "V.ICMS", oFont08N:oFont)
		nAuxH += aTamCol[11]
		oDanfe:Box(163, nAuxH, nColLim, nAuxH + aTamCol[12])
		oDanfe:Say(171, nAuxH + 2, "V.IPI", oFont08N:oFont)
		nAuxH += aTamCol[12]
		oDanfe:Box(163, nAuxH, nColLim, nAuxH + aTamCol[13])
		oDanfe:Say(171, nAuxH + 2, "A.ICMS", oFont08N:oFont)
		nAuxH += aTamCol[13]
		oDanfe:Box(163, nAuxH, nColLim, nAuxH + aTamCol[14])
		oDanfe:Say(171, nAuxH + 2, "A.IPI", oFont08N:oFont)
		
		//Verifico se ainda existem Dados Adicionais a serem impressos
		nLenMensagens:= Len(aMensagem)			
		IF (MV_PAR05 <> 1 .Or. (MV_PAR05 == 1 .And. lFimpar )).And. nMensagem <= nLenMensagens
			//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
			//³Dados Adicionais                                                        ³
			//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
			oDanfe:Say(719,000,"DADOS ADICIONAIS",oFont08N:oFont)
			oDanfe:Box(721,000,865,351)
			oDanfe:Say(729,002,"INFORMAÇÕES COMPLEMENTARES",oFont08N:oFont)				
			
			nLin:= 741
			nLenMensagens:= Len(aMensagem)
			--nMensagem
			For nX := 1 To Min(nLenMensagens - nMensagem, MAXMSG)				
				oDanfe:Say(nLin,002,aMensagem[nMensagem+nX],oFont08:oFont)
				nLin:= nLin+10
			Next nX
			nMensagem := nMensagem+nX
			
			oDanfe:Box(721+nLinhavers,350,865+nLinhavers,603)
			oDanfe:Say(729+nLinhavers,352,"RESERVADO AO FISCO",oFont08N:oFont)
			
			// Seta o máximo de itens para o MAXITEMP2
			nMaxItemP2 := MAXITEMP2
		Else
			// Seta o máximo de itens para o MAXITEMP2F
			nMaxItemP2 := MAXITEMP2F
		EndIF	
		If (!(nfolha-1)%2==0) .And. MV_PAR05==1
			If nY+69<nLenItens
				oDanfe:Say(875+nLinhavers,497,"CONTINUA NO VERSO")
			Endif
		End
		
		nL := 1
	EndIf
	
	nAuxH := 0
	
	If aAux[1][1][nY] == "-"
		oDanfe:Say(nLinha, nAuxH, Replicate("- ", 150), oFont08:oFont)
	Else
		oDanfe:Say(nLinha, nAuxH + 2, aAux[1][1][nY], oFont07:oFont )
		nAuxH += aTamCol[1]
		oDanfe:Say(nLinha, nAuxH + 2, aAux[1][2][nY], oFont07:oFont) // DESCRICAO DO PRODUTO
		nAuxH += aTamCol[2]
		oDanfe:Say(nLinha, nAuxH + 2, aAux[1][3][nY], oFont07:oFont) // NCM
		nAuxH += aTamCol[3]
		oDanfe:Say(nLinha, nAuxH + 2, aAux[1][4][nY], oFont07:oFont) // CST
		nAuxH += aTamCol[4]
		oDanfe:Say(nLinha, nAuxH + 2, aAux[1][5][nY], oFont07:oFont) // CFOP
		nAuxH += aTamCol[5]
		oDanfe:Say(nLinha, nAuxH + 2, aAux[1][6][nY], oFont07:oFont) // UN
		nAuxH += aTamCol[6]
		// Workaround para falha no FWMSPrinter:GetTextWidth()
		If Empty(aAux[1][6][nY]) // UN
			nAuxH2 := nAuxH + ((aTamCol[7] - 2) - RetTamTex(aAux[1][7][nY], oFont07:oFont, oDanfe)) + 2 //- RetTamTex("0", oFont08:oFont, oDanfe)
		Else
			nAuxH2 := nAuxH + ((aTamCol[7] - 2) - RetTamTex(aAux[1][7][nY], oFont07:oFont, oDanfe)) + 2
		EndIf
		oDanfe:Say(nLinha, nAuxH2 + 2, aAux[1][7][nY], oFont07:oFont) // QUANT
		nAuxH += aTamCol[7]
		If Empty(aAux[1][6][nY]) // UN
			nAuxH2 := nAuxH + ((aTamCol[8] - 2) - RetTamTex(aAux[1][8][nY], oFont07:oFont, oDanfe)) + 2 //- RetTamTex("0", oFont08:oFont, oDanfe)
		Else
			nAuxH2 := nAuxH + ((aTamCol[8] - 2) - RetTamTex(aAux[1][8][nY], oFont07:oFont, oDanfe)) + 2
		EndIf
		oDanfe:Say(nLinha, nAuxH2 + 2, aAux[1][8][nY], oFont07:oFont) // V UNITARIO
		nAuxH += aTamCol[8]		
		If Empty(aAux[1][6][nY]) // UN
			nAuxH2 := nAuxH + ((aTamCol[9] - 2) - RetTamTex(aAux[1][9][nY], oFont07:oFont, oDanfe)) + 2 //- RetTamTex("0", oFont08:oFont, oDanfe)
		Else
			nAuxH2 := nAuxH + ((aTamCol[9] - 2) - RetTamTex(aAux[1][9][nY], oFont07:oFont, oDanfe)) + 2
		EndIf
		oDanfe:Say(nLinha, nAuxH2 + 7, aAux[1][9][nY], oFont07:oFont) // V. TOTAL
		nAuxH += aTamCol[9]
		If Empty(aAux[1][6][nY]) // UN
			nAuxH2 := nAuxH + ((aTamCol[10] - 2) - RetTamTex(aAux[1][10][nY], oFont07:oFont, oDanfe)) + 2 //- RetTamTex("0", oFont08:oFont, oDanfe)
		Else
			nAuxH2 := nAuxH + ((aTamCol[10] - 2) - RetTamTex(aAux[1][10][nY], oFont07:oFont, oDanfe)) + 2
		EndIf
		oDanfe:Say(nLinha, nAuxH2 + 2, aAux[1][10][nY], oFont07:oFont) // BC. ICMS
		nAuxH += aTamCol[10]
		If Empty(aAux[1][6][nY]) // UN
			nAuxH2 := nAuxH + ((aTamCol[11] - 2) - RetTamTex(aAux[1][11][nY], oFont07:oFont, oDanfe)) + 2 //- RetTamTex("0", oFont08:oFont, oDanfe)
		Else
			nAuxH2 := nAuxH + ((aTamCol[11] - 2) - RetTamTex(aAux[1][11][nY], oFont07:oFont, oDanfe)) + 2
		EndIf
		oDanfe:Say(nLinha, nAuxH2 + 2, aAux[1][11][nY], oFont07:oFont) // V. ICMS
		nAuxH += aTamCol[11]
		If Empty(aAux[1][6][nY]) // UN
			nAuxH2 := nAuxH + ((aTamCol[12] - 2) - RetTamTex(aAux[1][12][nY], oFont07:oFont, oDanfe)) + 2 //- RetTamTex("0", oFont08:oFont, oDanfe)
		Else
			nAuxH2 := nAuxH + ((aTamCol[12] - 2) - RetTamTex(aAux[1][12][nY], oFont07:oFont, oDanfe)) + 2
		EndIf
		oDanfe:Say(nLinha, nAuxH2 + 2, aAux[1][12][nY], oFont07:oFont) // V.IPI
		nAuxH += aTamCol[12]
		nAuxH2 := nAuxH + ((aTamCol[13] - 2) - RetTamTex(aAux[1][13][nY], oFont07:oFont, oDanfe)) + 2
		oDanfe:Say(nLinha, nAuxH2 + 2, aAux[1][13][nY], oFont07:oFont) // A.ICMS
		nAuxH += aTamCol[13]
		nAuxH2 := nAuxH + ((aTamCol[14] - 2) - RetTamTex(aAux[1][14][nY], oFont07:oFont, oDanfe)) + 2
		oDanfe:Say(nLinha, nAuxH2 + 2, aAux[1][14][nY], oFont07:oFont) // A.IPI
	EndIf
	
	nLinha :=nLinha + 10
Next nY

nLenMensagens := Len(aMensagem)
While nMensagem <= nLenMensagens
	DanfeCpl(oDanfe,aItens,aMensagem,@nItem,@nMensagem,oNFe,oIdent,oEmitente,@nFolha,nFolhas,cCodAutSef,oNfeDPEC,cCodAutDPEC,cDtHrRecCab, cLogoD)
EndDo

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Finaliza a Impressão                                                    ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
If lPreview
	//	oDanfe:Preview()
EndIf

oDanfe:EndPage()

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Tratamento para nao imprimir DANFEs diferentes na mesma folha, uma na FRENTE e outra no VERSO.  |
//|   Isso quando a impressora estiver configurada para frente e verso                             ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
If MV_PAR05==1 .And. MV_PAR01 <> MV_PAR02 .And. (--nFolha)%2<>0
	oDanfe:StartPage()
	oDanfe:EndPage()
EndIf

Return(.T.)

/*
Private oNF        := oNFe:_NFe
Private oDPEC    :=oNfeDPEC
Default cCodAutSef := ""
Default cCodAutDPEC:= ""
Default cDtHrRecCab:= ""
Default dDtReceb   := CToD("")
*/
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Impressao do Complemento da NFe                                         ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
Static Function DanfeCpl(oDanfe,aItens,aMensagem,nItem,nMensagem,oNFe,oIdent,oEmitente,nFolha,nFolhas,cCodAutSef,oNfeDPEC,cCodAutDPEC,cDtHrRecCab, cLogoD)
Local nX            := 0
Local nLinha        := 0
Local nLenMensagens := Len(aMensagem)
Local nItemOld	    := nItem
Local nMensagemOld  := nMensagem
Local nForMensagens := 0
Local lMensagens    := .F.
Local cLogo      	:= SuperGetMV("MV_LOGONFE") //FisxLogo("1")
Local cChaveCont 	:= ""  
Local lConverte     := GetNewPar("MV_CONVERT",.F.)
Local lMv_Logod := If(GetNewPar("MV_LOGOD", "N" ) == "S", .T., .F.   )

If (nLenMensagens - (nMensagemOld - 1)) > 0
	lMensagens := .T.
	
EndIf

//ÚÄÄÄÄÄÄÄÄÄÄÄÄ------------------------ÄÄÄÄ¿
//³Dados Adicionais segunda parte em diante³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄ------------------------ÄÄÄÄÙ
If lMensagens
	nLenMensagens := Len(aMensagem)
	nForMensagens := Min(nLenMensagens, MAXITEMP2 + (nMensagemOld - 1) - (nItem - nItemOld))
	oDanfe:EndPage()
	oDanfe:StartPage()
	nLinha    :=180
	oDanfe:Say(160,000,"DADOS ADICIONAIS",oFont08N:oFont)
	oDanfe:Box(172,000,865,351)
	oDanfe:Say(170,002,"INFORMAÇÕES COMPLEMENTARES",oFont08N:oFont)
	oDanfe:Box(172,350,865,603)
	oDanfe:Say(170,352,"RESERVADO AO FISCO",oFont08N:oFont)
	
	oDanfe:Box(000,000,095,250)
	oDanfe:Say(010,098, "Identificação do emitente",oFont12N:oFont)
	nLinCalc	:=	023
	cStrAux		:=	AllTrim(NoChar(oEmitente:_xNome:Text,lConverte))
	nForTo		:=	Len(cStrAux)/25
	nForTo		+=	Iif(nForTo>Round(nForTo,0),Round(nForTo,0)+1-nForTo,nForTo)
	For nX := 1 To nForTo
		oDanfe:Say(nLinCalc,098,SubStr(cStrAux,Iif(nX==1,1,((nX-1)*25)+1),25), oFont12N:oFont )
		nLinCalc+=10
	Next nX
	
	cStrAux		:=	AllTrim(NoChar(oEmitente:_EnderEmit:_xLgr:Text,lConverte))+", "+AllTrim(oEmitente:_EnderEmit:_Nro:Text)
	nForTo		:=	Len(cStrAux)/32
	nForTo		+=	Iif(nForTo>Round(nForTo,0),Round(nForTo,0)+1-nForTo,nForTo)
	For nX := 1 To nForTo
		oDanfe:Say(nLinCalc,098,SubStr(cStrAux,Iif(nX==1,1,((nX-1)*32)+1),32),oFont08N:oFont)
		nLinCalc+=10
	Next nX
	
	If Type("oEmitente:_EnderEmit:_xCpl") <> "U"
		cStrAux		:=	"Complemento: "+AllTrim(NoChar(oEmitente:_EnderEmit:_xCpl:TEXT,lConverte))
		nForTo		:=	Len(cStrAux)/32
		nForTo		+=	Iif(nForTo>Round(nForTo,0),Round(nForTo,0)+1-nForTo,nForTo)
		For nX := 1 To nForTo
			oDanfe:Say(nLinCalc,098,SubStr(cStrAux,Iif(nX==1,1,((nX-1)*32)+1),32),oFont08N:oFont)
			nLinCalc+=10
		Next nX
		
		cStrAux		:=	AllTrim(NoChar(oEmitente:_EnderEmit:_xBairro:Text,lConverte))
		If Type("oEmitente:_EnderEmit:_Cep")<>"U"
			cStrAux		+=	" Cep:"+TransForm(oEmitente:_EnderEmit:_Cep:Text,"@r 99999-999")
		EndIf
		nForTo		:=	Len(cStrAux)/32
		nForTo		+=	Iif(nForTo>Round(nForTo,0),Round(nForTo,0)+1-nForTo,nForTo)
		For nX := 1 To nForTo
			oDanfe:Say(nLinCalc,098,SubStr(cStrAux,Iif(nX==1,1,((nX-1)*32)+1),32),oFont08N:oFont)
			nLinCalc+=10
		Next nX
	 	oDanfe:Say(nLinCalc,098, oEmitente:_EnderEmit:_xMun:Text+"/"+oEmitente:_EnderEmit:_UF:Text,oFont08N:oFont)
	 	nLinCalc+=10
	 	oDanfe:Say(nLinCalc,098, "Fone: "+cTel,oFont08N:oFont)
	Else
		oDanfe:Say(nLinCalc,098, oEmitente:_EnderEmit:_xBairro:Text+" Cep:"+TransForm(IIF(Type("oEmitente:_EnderEmit:_Cep")=="U","",oEmitente:_EnderEmit:_Cep:Text),"@r 99999-999"),oFont08N:oFont)
		nLinCalc+=10
		oDanfe:Say(nLinCalc,098, oEmitente:_EnderEmit:_xMun:Text+"/"+oEmitente:_EnderEmit:_UF:Text,oFont08N:oFont)
		nLinCalc+=10
		oDanfe:Say(nLinCalc,098, "Fone: "+cTel,oFont08N:oFont)
	EndIf
	
	oDanfe:Box(000,248,095,351)
	oDanfe:Say(013,255, "DANFE",oFont18N:oFont)
	oDanfe:Say(023,255, "DOCUMENTO AUXILIAR DA",oFont07:oFont)
	oDanfe:Say(033,255, "NOTA FISCAL ELETRÔNICA",oFont07:oFont)
	oDanfe:Say(043,255, "0-ENTRADA",oFont08:oFont)
	oDanfe:Say(053,255, "1-SAÍDA"  ,oFont08:oFont)
	oDanfe:Box(037,305,047,315)
	oDanfe:Say(045,307, oIdent:_TpNf:Text,oFont08N:oFont)
	oDanfe:Say(062,255,"N. "+StrZero(Val(oIdent:_NNf:Text),9),oFont10N:oFont)
	oDanfe:Say(072,255,"SÉRIE "+oIdent:_Serie:Text,oFont10N:oFont)
	oDanfe:Say(082,255,"FOLHA "+StrZero(nFolha,2)+"/"+StrZero(nFolhas,2),oFont10N:oFont)
	
	oDanfe:Box(000,350,095,603)
	oDanfe:Box(000,350,040,603)
	oDanfe:Box(040,350,062,603)
	oDanfe:Box(063,350,095,603)
	oDanfe:Say(058,355,TransForm(SubStr(oNF:_InfNfe:_ID:Text,4),"@r 9999 9999 9999 9999 9999 9999 9999 9999 9999 9999 9999"),oFont12N:oFont)
	
	oDanfe:Say(048,355,"CHAVE DE ACESSO DA NF-E",oFont12N:oFont)
	nFontSize := 28
	oDanfe:Code128C(036,370,SubStr(oNF:_InfNfe:_ID:Text,4), nFontSize )
	
	If lMv_Logod
		oDanfe:SayBitmap(001,001,cLogoD,094,093)
	Else
		oDanfe:SayBitmap(001,001,cLogo,094,093)
	EndIf
	
	If Empty(cChaveCont)
		oDanfe:Say(075,355,"Consulta de autenticidade no portal nacional da NF-e",oFont12:oFont)
		oDanfe:Say(085,355,"www.nfe.fazenda.gov.br/portal ou no site da SEFAZ Autorizada",oFont12:oFont)
	Endif
	
	If  !Empty(cCodAutDPEC)
		oDanfe:Say(075,355,"Consulta de autenticidade no portal nacional da NF-e",oFont12:oFont)
		oDanfe:Say(085,355,"www.nfe.fazenda.gov.br/portal ou no site da SEFAZ Autorizada",oFont12:oFont)
	Endif
	
	
	If nFolha == 1
		If !Empty(cCodAutDPEC)
			nFontSize := 28
			oDanfe:Code128C(093,370,cCodAutDPEC, nFontSize )
		Endif
	Endif
	
	// inicio do segundo codigo de barras ref. a transmissao CONTIGENCIA OFF LINE
	If !Empty(cChaveCont) .And. Empty(cCodAutDPEC) .And. !(Val(oNF:_INFNFE:_IDE:_SERIE:TEXT) >= 900)
		If nFolha == 1
			If !Empty(cChaveCont)
				nFontSize := 28
				oDanfe:Code128C(093,370,cChaveCont, nFontSize )
			EndIf
		Else
			If !Empty(cChaveCont)
				nFontSize := 28
				oDanfe:Code128C(093,370,cChaveCont, nFontSize )
			EndIf
		EndIf
	EndIf
	
	oDanfe:Box(100,000,123,603)
	oDanfe:Box(100,000,123,300)
	oDanfe:Say(109,002,"NATUREZA DA OPERAÇÃO",oFont08N:oFont)
	oDanfe:Say(119,002,oIdent:_NATOP:TEXT,oFont08:oFont)
	If(!Empty(cCodAutDPEC))
		oDanfe:Say(109,300,"NÚMERO DE REGISTRO DPEC",oFont08N:oFont)
	Endif
	If(((Val(oNF:_INFNFE:_IDE:_SERIE:TEXT) >= 900).And.(oNFe:_NFE:_INFNFE:_IDE:_TPEMIS:TEXT)$"2") .Or. (oNFe:_NFE:_INFNFE:_IDE:_TPEMIS:TEXT)$"1")
		oDanfe:Say(109,302,"PROTOCOLO DE AUTORIZAÇÃO DE USO",oFont08N:oFont)
	Endif
	If((oNFe:_NFE:_INFNFE:_IDE:_TPEMIS:TEXT)$"25")
		oDanfe:Say(109,300,"DADOS DA NF-E",oFont08N:oFont)
	Endif
	oDanfe:Say(119,302,IIF(!Empty(cCodAutDPEC),cCodAutDPEC+" "+AllTrim(ConvDate(oNF:_InfNfe:_IDE:_DEMI:Text))+" "+AllTrim(cDtHrRecCab),IIF(!Empty(cCodAutSef) .And. ((Val(oNF:_INFNFE:_IDE:_SERIE:TEXT) >= 900).And.(oNFe:_NFE:_INFNFE:_IDE:_TPEMIS:TEXT)$"23") .Or. (oNFe:_NFE:_INFNFE:_IDE:_TPEMIS:TEXT)$"1",cCodAutSef+" "+AllTrim(ConvDate(oNF:_InfNfe:_IDE:_DEMI:Text))+" "+AllTrim(cDtHrRecCab),TransForm(cChaveCont,"@r 9999 9999 9999 9999 9999 9999 9999 9999 9999"))),oFont08:oFont)
	nFolha++
	
	oDanfe:Box(126,000,153,603)
	oDanfe:Box(126,000,153,200)
	oDanfe:Box(126,200,153,400)
	oDanfe:Box(126,400,153,603)
	oDanfe:Say(135,002,"INSCRIÇÃO ESTADUAL",oFont08N:oFont)
	oDanfe:Say(143,002,IIf(Type("oEmitente:_IE:TEXT")<>"U",oEmitente:_IE:TEXT,""),oFont08:oFont)
	oDanfe:Say(135,205,"INSC.ESTADUAL DO SUBST.TRIB.",oFont08N:oFont)
	oDanfe:Say(143,205,IIf(Type("oEmitente:_IEST:TEXT")<>"U",oEmitente:_IEST:TEXT,""),oFont08:oFont)
	oDanfe:Say(135,405,"CNPJ",oFont08N:oFont)
	oDanfe:Say(143,405,TransForm(oEmitente:_CNPJ:TEXT,IIf(Len(oEmitente:_CNPJ:TEXT)<>14,"@r 999.999.999-99","@r 99.999.999/9999-99")),oFont08:oFont)
	
	For nX := nMensagem To nForMensagens
		oDanfe:Say(nlinha,002,aMensagem[nX],oFont08:oFont)
		nMensagem++
		nLinha:= nLinha+ 10
	Next nX
EndIf
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Finalizacao da pagina do objeto grafico                                 ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
oDanfe:EndPage()

Return(.T.)

Static Function ConvDate(cData)

Local dData
cData  := StrTran(cData,"-","")
dData  := Stod(cData)
Return PadR(StrZero(Day(dData),2)+ "/" + StrZero(Month(dData),2)+ "/" + StrZero(Year(dData),4),15)

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³DANFE     ºAutor  ³Marcos Taranta      º Data ³  10/01/09   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDesc.     ³Pega uma posição (nTam) na string cString, e retorna o      º±±
±±º          ³caractere de espaço anterior.                               º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP                                                        º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
Static Function EspacoAt(cString, nTam)

Local nRetorno := 0
Local nX       := 0

/**
 * Caso a posição (nTam) for maior que o tamanho da string, ou for um valor
 * inválido, retorna 0.
 */
If nTam > Len(cString) .Or. nTam < 1
	nRetorno := 0
	Return nRetorno
EndIf

/**
 * Procura pelo caractere de espaço anterior a posição e retorna a posição
 * dele.
 */
nX := nTam
While nX > 1
	If Substr(cString, nX, 1) == " "
		nRetorno := nX
		Return nRetorno
	EndIf
	
	nX--
EndDo

/**
 * Caso não encontre nenhum caractere de espaço, é retornado 0.
 */
nRetorno := 0

Return nRetorno

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³DANFE     ºAutor  ³Fabio Santana	      º Data ³  04/10/10   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDesc.     ³Converte caracteres espceiais						                         º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP                                                         º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*/
STATIC FUNCTION NoChar(cString,lConverte)

Default lConverte := .F.

If lConverte
	cString := (StrTran(cString,"&lt;","<"))
	cString := (StrTran(cString,"&gt;",">"))
	cString := (StrTran(cString,"&amp;","&"))
	cString := (StrTran(cString,"&quot;",'"'))
	cString := (StrTran(cString,"&#39;","'"))
EndIf

Return(cString)

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³DANFEIII  ºAutor  ³Microsiga           º Data ³  12/17/10   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDesc.     ³ Tratamento para o código do item                           º±±
±±º          ³                                                            º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP                                                        º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
STATIC FUNCTION MaxCod(cString,nTamanho)

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Tratamento para saber quantos caracteres irão caber na linha ³
//³ visto que letras ocupam mais espaço do que os números.      ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

Local nMax	:= 0
Local nY   	:= 0
Default nTamanho := 45

For nMax := 1 to Len(cString)
	If IsAlpha(SubStr(cString,nMax,1)) .And. SubStr(cString,nMax,1) $ "MOQW"  // Caracteres que ocupam mais espaço em pixels
		nY += 7
	Else
		nY += 5
	EndIf
	
	If nY > nTamanho   // é o máximo de espaço para uma coluna
		nMax--
		Exit
	EndIf
Next

Return nMax

//-----------------------------------------------------------------------
/*/{Protheus.doc} RetTamCol
Retorna um array do mesmo tamanho do array de entrada, contendo as
medidas dos maiores textos para cálculo de colunas.

@author Marcos Taranta
@since 24/05/2011
@version 1.0 

@param  aCabec     Array contendo as strings de cabeçalho das colunas
        aValores   Array contendo os valores que serão populados nas
                   colunas.
        oPrinter   Objeto de impressão instanciado para utilizar o método
                   nativo de cálculo de tamanho de texto.
        oFontCabec Objeto da fonte que será utilizada no cabeçalho.
        oFont      Objeto da fonte que será utilizada na impressão.

@return aTamCol  Array contendo os tamanhos das colunas baseados nos
                 valores.
/*/
//-----------------------------------------------------------------------
Static Function RetTamCol(aCabec, aValores, oPrinter, oFontCabec, oFont)
	
	Local aTamCol    := {}
	Local nAux       := 0

	Local nX         := 0
	Local nY         := 0
	                          
	Local oFontSize	 := FWFontSize():new()
	
	For nX := 1 To Len(aCabec)
		
		AADD(aTamCol, {})
		//aTamCol[nX] := Round(oPrinter:GetTextWidth(aCabec[nX], oFontCabec) * nConsNeg + 4, 0)
		aTamCol[nX] := oFontSize:getTextWidth( alltrim(aCabec[nX]), oFontCabec:Name, oFontCabec:nWidth, oFontCabec:Bold, oFontCabec:Italic )
		
	Next nX
	
	For nX := 1 To Len(aValores[1])
		
		nAux := 0
		
		For nY := 1 To Len(aValores[1][nX])
			
			If (oPrinter:GetTextWidth(aValores[1][nX][nY], oFont) * nConsTex) > nAux
				//nAux := Round(oPrinter:GetTextWidth(aValores[1][nX][nY], oFont) * nConsTex + 4, 0)
				nAux := oFontSize:getTextWidth( Alltrim(aValores[1][nX][nY]), oFontCabec:Name, oFontCabec:nWidth, oFontCabec:Bold, oFontCabec:Italic )
			EndIf
			
		Next nY
		
		If aTamCol[nX] < nAux
			aTamCol[nX] := nAux
		EndIf
		
	Next nX
	
	// Checa se os campos completam a página, senão joga o resto na coluna da
	//   descrição de produtos/serviços
	nAux := 0
	For nX := 1 To Len(aTamCol)
		
		nAux += aTamCol[nX]
		
	Next nX
	If nAux < 603
		aTamCol[2] += 603 - nAux
	EndIf                       
	If nAux > 603               
		aTamCol[2] -= nAux - 603 
	EndIf
	
Return aTamCol

//-----------------------------------------------------------------------
/*/{Protheus.doc} RetTamTex
Retorna o tamanho em pixels de uma string. (Workaround para o GetTextWidth)

@author Marcos Taranta
@since 24/05/2011
@version 1.0 

@param  cTexto   Texto a ser medido.
        oFont    Objeto instanciado da fonte a ser utilizada.
        oPrinter Objeto de impressão instanciado.

@return nTamanho Tamanho em pixels da string.
/*/
//-----------------------------------------------------------------------
Static Function RetTamTex(cTexto, oFont, oPrinter)
	
	Local nTamanho := 0
	Local oFontSize:= FWFontSize():new() 
	
	//nTamanho := oPrinter:GetTextWidth(cTexto, oFont)
	nTamanho := oFontSize:getTextWidth( cTexto, oFont:Name, oFont:nWidth, oFont:Bold, oFont:Italic )
	
  	nTamanho := Round(nTamanho, 0)
	
Return nTamanho

//-----------------------------------------------------------------------
/*/{Protheus.doc} PosQuebrVal
Retorna a posição onde um valor deve ser quebrado

@author Marcos Taranta
@since 27/05/2011
@version 1.0 

@param  cTexto Texto a ser medido.

@return nPos   Posição aonde o valor deve ser quebrado.
/*/
//-----------------------------------------------------------------------
Static Function PosQuebrVal(cTexto)
	
	Local nPos := 0
	
	If Empty(cTexto)
		Return 0
	EndIf
	
	If Len(cTexto) <= MAXVALORC
		Return Len(cTexto)
	EndIf
	
	If SubStr(cTexto, MAXVALORC, 1) $ ",."
		nPos := MAXVALORC - 2
	Else
		nPos := MAXVALORC
	EndIf
	
Return nPos

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³PREVNFE   ºAutor  ³Data Manager        º Data ³  04/10/10   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDesc.     ³                                                            º±±
±±º          ³                                                            º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP                                                        º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
Static Function StrGetAlphas( cString )
Return StrTran(cString , "&amp;" , "&" )

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³PREVNFE   ºAutor  ³Data Manager        º Data ³  04/10/10   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDesc.     ³                                                            º±±
±±º          ³                                                            º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP                                                        º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
Static Function OpenTable(cAlias)
Local 	aCampos := {}
Local  aIndices := {}
Local  cDriver := RDDSPED
Local  nx := 0

 //ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
 //³ Abertura de tabelas                                  ³
 //ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

	aadd(aCampos,{"ID_ENT"    ,"C",006,0})
	aadd(aCampos,{"PARAMETRO" ,"C",010,0})
	aadd(aCampos,{"CONTEUDO"  ,"C",250,0})

 aadd(aIndices,{"ID_ENT+PARAMETRO","PK"})

 Use &(cAlias) Alias &(cAlias) SHARED NEW Via cDriver
 nX := 1
 While nX <= 10 .And. NetErr() .And. !KillApp()
 	Sleep(1000)
 	Sleep(1000)
 	Use &(cAlias) Alias &(cAlias) SHARED NEW Via cDriver
 	nX++
 EndDo
 
 //ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
 //³ Abertura de indices                                  ³
 //ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
 If cDriver == "TOPCONN"
 	cOrd := "00"
 	For nX := 1 To Len(aIndices)
 		cOrd := Soma1(cOrd,2)
 		cOrdName := cAlias+cOrd
 		DbSetIndex(cOrdName)
 		DbSetNickName(OrdName(nX),cOrdName)
 	Next nX
 Else
 	nX   := 1
 	cOrd := "00"
 	While ( ! Empty(OrdName(nX)) )
 		cOrdName := cAlias+cOrd
 		If ( nX > Len(aIndices) )
 			ConOut("Index OF "+cAlias+" Corrupted")
 			Ms_Quit()
 		EndIf
 		DbSetNickName(OrdName(nX),cOrdName)
 		nX++
 	EndDo
 EndIf
 DbSetOrder(1)

Return()

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Funcao    ³SpedNfeCon³ Rev.  ³Eduardo Riera          ³ Data ³27.07.2007³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡…o ³Funcao de conversao do XML Totvs para o padrao da SEFAZ     ³±±
±±³          ³configurado                                                 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Parametros³ExpC1: XML                                                  ³±±
±±³          ³ExpC2: Codigo da Entidade                                   ³±±
±±³          ³ExpC3: Email de transmissao da mensagem                (OPC)³±±
±±³          ³ExpL4: .F. - De Totvs para NFe                         (OPC)³±±
±±³          ³       .T. - De Nfe para Totvs                         (OPC)³±±
±±³          ³ExpC5: Descricao do erro                               (REF)³±±
±±³          ³ExpC6: Modelo do Layout                                (REF)³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Retorno   ³ExpL1: Indica se o XML foi convertido corretamente          ³±±
±±³          ³                                                            ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡„o ³Esta funcao tem como objetivo converter o padrao de XML da  ³±±
±±³          ³totvs para o padrão da SEFAZ. Se o XML enviado nao estiver  ³±±
±±³          ³no padrao totvs, sera apenas verificada se é um Schema vali-³±±
±±³          ³do na SEFAZ.                                                ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Observacao³                                                            ³±±
±±³          ³                                                            ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Uso       ³ Totvs SPED Services Gateway                                ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/
Static Function SpedNfeConv(cXML,cIdEnt,cMail,lInverso,cErroSoap,cNFMod,aRespNfe)

Local aProd     := {}
Local lRetorno  := .T.
Local lCdata	:= .F.
Local cDirSchema:= IIf(IsSrvUnix(),"/schemas/", "\schemas\")
Local cNewXML   := ""
Local cAviso    := ""
Local cErro     := ""
Local nX        := 0
Local aImp      := {{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0,0,0,0}}
Local aTot      := {0,0,0,0}
Local lBreak    := .F.
Local bErro     := Nil
Local nHandle   := 0
Local cDepc 	:=""
Local cStr		:=""
Local nStr		:=0
Local lDelMail  := AllTrim(GetSrvProfString("SPED_DELMAIL","0"))=="1"
Local nY        := 0
Local nV        := 0
Local nW        := 0
Local nZ        := 0
Local cChave    := ""
Local nAmbiente := 0
Local cURL      := ""
Private cString := ""
Private oNFe
DEFAULT cMail   := ""
DEFAULT lInverso:= .F.
DEFAULT aRespNfe:= {}

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Verifico se foi enviado alguma clausula CDATA                           ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
lCdata := IIF("![CDATA[" $ cXML,.T.,.F.)
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Verifica se foi recebido um XML valido                                  ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
oNFe := XmlParser(cXML,"_",@cAviso,@cErro)
If Empty(cAviso) .And. Empty(cErro)
	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³Montagem do novo XML                                                    ³
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	If Type("oNFe:_TC_RPS")=="U" .And. Type("oNFe:_RPS")=="U"
		If Type("oNFe:_NFE:_INFNFE:_VERSAO:TEXT")=="U" .And. Type("oNFe:_CTE:_INFCTE:_VERSAO:TEXT")=="U"
		    
   If Type("oNFe:_TC_INFRPS")=="U" .And. Type("oNFe:_INFRPS")=="U"
    If oNFe:_INFNFE:_VERSAO:TEXT=="T02.00"
    	cNFMod := AllTrim(oNFe:_INFNFE:_MODELO:TEXT)
    Else
    	cNFMod := "55"
    EndIf
			Else
				cNFMod := "56"
			EndIf

   Do Case
   	Case cNFMod == "57"
   		cXml     := XMLSaveStr(oNFe:_INFNFE:_CTE,.F.)
   		lRetorno := .T.
   		cMail    := "" //Provisorio
   	Case cNFMod == "56"
 				nAmbiente := Val(SpedGetMv("MV_AMBINSE",SPED001->ID_ENT))
					cURL := GetURLNSe(GetMunCod(SPED001->ID_ENT),Str(nAmbiente,1))
					lRetorno := .T.
					
					If "/bhiss-ws/"$cURL
						cXml := XMLSaveStr(oNFe:_INFRPS,.F.)
	  			cChave := GetUFCode(SPED001->UF)+SubStr(oNFe:_INFRPS:_DATAEMISSAO:TEXT,3,2)+SubStr(oNFe:_INFRPS:_DATAEMISSAO:TEXT,6,2)+Alltrim(SPED001->CNPJ)+"56"+StrZero(Val(oNFe:_INFRPS:_IDENTIFICACAORPS:_SERIE:TEXT),3)+StrZero(Val(oNFe:_INFRPS:_IDENTIFICACAORPS:_NUMERO:TEXT),9)+StrZero(Val(oNFe:_INFRPS:_IDENTIFICACAORPS:_NUMERO:TEXT),9)
	  			nY := At(">",cXml)//Posição inicial da tag InfRps
	  			nV := At("</InfRps>",cXml)//Posição final da tag InfRps
	  			cNewXML := '<InfRps Id="NSe'+cChave+'" xmlns="http://www.abrasf.org.br/nfse.xsd"'
		    cNewXML := cNewXML + SubStr(cXml,nY,nV-nY)+'</InfRps>'//Conteudo da Tag 
						cXml := cNewXML
					Elseif "issnetonline"$cURL
						cXml := XMLSaveStr(oNFe:_TC_INFRPS,.F.)
	  			cChave := GetUFCode(SPED001->UF)+SubStr(oNFe:_TC_INFRPS:_TC_DATAEMISSAO:TEXT,3,2)+SubStr(oNFe:_TC_INFRPS:_TC_DATAEMISSAO:TEXT,6,2)+Alltrim(SPED001->CNPJ)+"56"+StrZero(Val(oNFe:_TC_INFRPS:_TC_IDENTIFICACAORPS:_TC_SERIE:TEXT),3)+StrZero(Val(oNFe:_TC_INFRPS:_TC_IDENTIFICACAORPS:_TC_NUMERO:TEXT),9)+StrZero(Val(oNFe:_TC_INFRPS:_TC_IDENTIFICACAORPS:_TC_NUMERO:TEXT),9)
	  			nY := At(">",cXml)//Posição inicial da tag InfRps
	  			nV := At("</tc:InfRps>",cXml)//Posição final da tag InfRps
	  			cNewXML := '<tc:InfRps Id="NSe'+cChave+'" xmlns="http://www.issnetonline.com.br/webserviceabrasf/vsd/tipos_complexos.xsd" xmlns:tc="http://www.issnetonline.com.br/webserviceabrasf/vsd/tipos_complexos.xsd"'
		    cNewXML := cNewXML + SubStr(cXml,nY,nV-nY)+'</tc:InfRps>'//Conteudo da Tag 
						cXml := cNewXML
						
						If nAmbiente=2//Se for ambiente de teste a ISSNET só aceita o código 999 para o municipio de prestacao de serviço
	   			nW := At("<tc:MunicipioPrestacaoServico>",cXml)//Posição inicial da tag MunicipioPrestacaoServico
	   			nZ := At("</tc:MunicipioPrestacaoServico>",cXml)//Posição final da tag MunicipioPrestacaoServico
	   			cNewXML := SubStr(cXml,1,nW-1)+'<tc:MunicipioPrestacaoServico>999'//Conteudo inicio do XML 
			    cNewXML := cNewXML + SubStr(cXml,nZ,Len(cXml))//Conteudo da Tag 
							cXml := cNewXML
						EndIf
						
					Else
						lRetorno := .F.
					EndIf
					
	  		cMail    := "" //Provisorio
	  	OtherWise
	  		bErro     := ErrorBlock({|e| lBreak := .T. ,ErrNfeConv(e,cXML,cNewXML+cString,@cErroSoap)})
					Begin Sequence
						oNFe := oNFe:_infNFe
						cNewXML := ""
						cNewXML := XmlNfeIde(lInverso,cVersao,cIdEnt,oNFe:_Ide,oNFe:_Emit,IIf(Type("oNFe:_Cobr")=="U",Nil,oNFe:_Cobr))
						cNewXML += XmlNfeEmit(cVersao,oNFe:_Emit)
						cNewXML += XmlNfeDest(cVersao,oNFe:_Dest,@cMail)
						cNewXML += XmlNfeRetirada(cVersao,IIf(Type("oNFe:_Retirada")=="U",Nil,oNFe:_Retirada))
						cNewXML += XmlNfeEntrega(cVersao,IIf(Type("oNFe:_Entrega")=="U",Nil,oNFe:_Entrega))
						
						If ValType(oNfe:_Det)=="A"
							aProd := oNfe:_Det
						Else
							aProd := {oNfe:_Det}
						EndIf
						For nX := 1 To Len(aProd)
							cNewXML += XmlNfeItem(cVersao,aProd[nX],@aImp,@aTot,lCdata)
						Next nX
						cNewXml += XmlNfeTotal(cVersao,oNfe:_Total,@aImp,@aTot)
						cNewXml += XmlNfeTransp(cVersao,oNfe:_Transp)
						cNewXml += XmlNfeCob(cVersao,IIf(Type("oNFe:_Cobr")=="U",Nil,oNFe:_Cobr))
						cNewXml += XmlNfeInf(cVersao,IIf(Type("oNFe:_InfAdic")=="U",Nil,oNFe:_InfAdic),lCdata)
						cNewXml += XmlNfeExp(cVersao,IIf(Type("oNFe:_exporta")=="U",Nil,oNFe:_exporta))
						cNewXml += XmlNfeInfCompra(cVersao,IIf(Type("oNFe:_Compra")=="U",Nil,oNFe:_Compra))
						cNewXml += XmlNfeCana(cVersao,IIf(Type("oNFe:_cana")=="U",Nil,oNFe:_cana))
					
						cNewXml += "</infNFe>"
						cNewXml := '<NFe xmlns="http://www.portalfiscal.inf.br/nfe">'+cNewXML			
						cNewXml += "</NFe>"	
					
						If !lBreak
							cXML    :=cNewXML
							If !Type("oNFe:_INFADIC:_CPL")=="U"
								nX := At("[CONTRTSS=",UPPER(cXml))
								If nX > 0
									cStr	:=	SubStr(cXml,nX+10)
									aAdd(aRespNfe,SToD(StrTran(SubStr(cStr,1,At("#",cStr)-1),"-","")))
								
									cStr	:=	SubStr(cStr,At("#",cStr)+1)
									aAdd(aRespNfe,SubStr(cStr,1,At("#",cStr)-1))
									
									cStr	:=	SubStr(cStr,At("#",cStr)+1)
									nStr	:=	At("]",cStr)
									aAdd(aRespNfe,SubStr(cStr,1,nStr-1))
									cXml	:=	SubStr(cXml,1,nX-1)+SubStr(cStr,nStr+1)
									cXml 	:= StrTran(cXml,"<infAdic><infAdFisco></infAdFisco><infCpl></infCpl></infAdic>","")
									cXml 	:= StrTran(cXml,"<infAdic><infAdFisco></infAdFisco></infAdic>","")
									cXml 	:= StrTran(cXml,"<infAdic><infCpl></infCpl></infAdic>","")
									cXml 	:= StrTran(cXml,"<infAdic>></infCpl></infAdic>","")
									cXml 	:= StrTran(cXml,"<infAdic></infAdic>","")									
									cXml 	:= StrTran(cXml,"<infCpl></infCpl>","")								
								EndIf 
							EndIf 
						Else
							Break
						EndIf
					Recover
						cXml := cNewXML
						lRetorno := .F.
					End Sequence
					ErrorBlock (bErro)
			EndCase
		Else 
			nX := At("[EMAIL=",UPPER(cXml))
			If nX > 0
				cMail := ""
				cMail := SubStr(cXml,nX+7)
				nX := At("]",UPPER(cMail))
				cMail := SubStr(cMail,1,nX-1)
				If lDelMail
					cXml := StrTran(cXml,"[EMAIL="+cMail+"]","")
				EndIf
				cMail :=AllTrim(cMail)
	        EndIf
			nX := At("[CONTRTSS=",UPPER(cXml))
			If nX > 0
				cStr	:=	SubStr(cXml,nX+10)
				aAdd(aRespNfe,SToD(StrTran(SubStr(cStr,1,At("#",cStr)-1),"-","")))
			
				cStr	:=	SubStr(cStr,At("#",cStr)+1)
				aAdd(aRespNfe,SubStr(cStr,1,At("#",cStr)-1))
				
				cStr	:=	SubStr(cStr,At("#",cStr)+1)
				nStr	:=	At("]",cStr)
				aAdd(aRespNfe,SubStr(cStr,1,nStr-1))
				cXml	:=	SubStr(cXml,1,nX-1)+SubStr(cStr,nStr+1)
			EndIf 
			
			cXml := StrTran(cXml,"<infAdic><infAdFisco></infAdFisco><infCpl></infCpl></infAdic>","")
			cXml := StrTran(cXml,"<infAdic><infAdFisco></infAdFisco></infAdic>","")
			cXml := StrTran(cXml,"<infAdic><infCpl></infCpl></infAdic>","")
			cXml := StrTran(cXml,"<infAdic></infAdic>","")
			
			Do Case
				Case Type("oNFe:_CTE:_INFCTE:_VERSAO:TEXT")<>"U"
					cNFMod := "57"
				OtherWise
					cNFMod := "55"		
			EndCase
		EndIf
	Else
		cNFMod := "56"
	EndIf
Else
	cErroSoap := cErro+cAviso
	lRetorno := .F.
EndIf

If cAmbiente =="2"
	cMail := "nfe@pelmexms.com.br"
EndIf

Return(lRetorno)

Static Function ErrNfeConv(oErro,cOrigXML,cNewXML,cErro)

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Montagem da mensagem de erro                                            ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
cErro:= "Mensagem de erro: "+oErro:Description+CRLF
cErro+= ProcName(2)+": "+AllTrim(Str(ProcLine(2),18))+CRLF
cErro+= CRLF
cErro+= "XML recebido: "+cOrigXML+CRLF
cErro+= "XML convertido: "+cNewXML+CRLF
cErro+= CRLF

Break

Return(cErro)

Static Function XmlNfeIde(lInverso,cVersao,cIdEnt,oIde,oEmit,oCobr)

Local aNfVinc := {}
Local cIndPag := ""
Local cNFRef  := ""
Local cModal  := IIF(cModalidad == "1","N","C")
Local cDhcont := ""
Local nModal  := cModalidad
Local nX      := 0
local nAutoriz:= 0
Local lConting:= .F.
Private oXml  := oCobr
Private oXmlVinc
cString := ""

If nModal == "4"
	nModal := "1"
EndIf

Do case 
 Case nModal =="1"
		nAutoriz := "1"
		
	Case nModal == "2"
		nAutoriz := "2"
		
	Case nModal == "3"
		nAutoriz := "3"
		
	Case nModal $ "4"
		nAutoriz := "1"
		
	Case nModal == "5"
		nAutoriz := "4"
		
	Case nModal == "6"
		nAutoriz := "1"
		
	Case nModal == "7"
		nAutoriz := "5"
EndCase
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Calcula a chave de acesso sem o digito verificador                      ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
If !lInverso
	If cVersao < "1.11"
		cChave := GetUFCode(oEmit:_EnderEmit:_UF:TEXT)+SubStr(oIde:_dEmi:TEXT,3,2)+SubStr(oIde:_dEmi:TEXT,6,2)+oEmit:_CNPJ:TEXT+"55"+StrZero(Val(oIde:_Serie:TEXT),3)+StrZero(Val(oIde:_nNF:TEXT),9)+StrZero(Val(oIde:_cNF:TEXT),9)
	Else   	
 	cChave := GetUFCode(oEmit:_EnderEmit:_UF:TEXT)+SubStr(oIde:_dEmi:TEXT,3,2)+SubStr(oIde:_dEmi:TEXT,6,2)+oEmit:_CNPJ:TEXT+"55"+StrZero(Val(oIde:_Serie:TEXT),3)+StrZero(Val(oIde:_nNF:TEXT),9)+nAutoriz+StrZero(Val(oIde:_cNF:TEXT),8)
	EndIf 																																								   										    
	
	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³Verifica o tipo de pagamento                                            ³
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	oXml  := oIde
	If Type("oXml:_indpag")<>"U"
		cIndPag := oIde:_indpag:TEXT
	Else
		oXml  := oCobr
		Do Case
			Case oXml==Nil
				cIndPag := "2"
			Case ValType(oXml:_Dup)=="A"
				If Len(oXml:_Dup)==1 .And. oXml:_Dup[01]:_DtVenc:TEXT <= oIde:_dEmi:TEXT
					cIndPag := "0"
				Else
					cIndPag := "1"
				EndIf
			OtherWise
				If oXml:_Dup:_DtVenc:TEXT <= oIde:_dEmi:TEXT
					cIndPag := "0"
				Else
					cIndPag := "1"
				EndIf
		EndCase
	EndIf
	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³Monta o XML                                                             ³
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	oXml := oIde
	cString += '<infNFe versao="'+cVersao+'" Id="NFe'+cChave+Modulo11(cChave)+'">'
	cString += '<ide>'
	cString += '<cUF>'+GetUFCode(oEmit:_EnderEmit:_UF:TEXT)+'</cUF>'
	If cVersao < "1.11"
		cString += '<cNF>'+ConvType(StrZero(Val(oIde:_cNF:TEXT),9,0),9,0)+'</cNF>'
	Else
		cString += '<cNF>'+ConvType(StrZero(Val(oIde:_cNF:TEXT),8,0),8,0)+'</cNF>'
	EndIf		
	cString += '<natOp>'+oIde:_natOp:TEXT+'</natOp>'
	cString += '<indPag>'+cIndPag+'</indPag>'
	cString += '<mod>55</mod>'
	cString += '<serie>'+oIde:_Serie:TEXT+'</serie>'
	cString += '<nNF>'+oIde:_nNF:TEXT+'</nNF>'
	cString += '<dEmi>'+oIde:_dEmi:TEXT+'</dEmi>
	cString += NfeTag('<dSaiEnt>',"oXml:_dSaiEnt:TEXT")
	If cVersao >= "2.00"
		cString += NfeTag('<hSaiEnt>',"oXml:_hSaiEnt:TEXT")
	EndIF                                               
	cString += '<tpNF>'+oIde:_tpNF:TEXT+'</tpNF>'
	If Type("oXml:_cMunFG")<>"U"
		cString += '<cMunFG>'+oIde:_cMunFG:TEXT+'</cMunFG>'
	Else
		cString += '<cMunFG>'+oEmit:_EnderEmit:_cMun:TEXT+'</cMunFG>'
	EndIf
	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³Verifica se ha notas referenciadas                                      ³
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	If Type("oXml:_NFRef")<>"U"
		If Type("oXml:_NFRef:_refNFe")<>"U"
			If ValType(oXml:_NFRef:_refNFe)=="A"
				aNfVinc := oXml:_NFRef:_refNFe
			Else
				aNfVinc := {oXml:_NFRef:_refNFe}
			EndIf
			For nX := 1 To Len(aNfVinc)
				cString += "<NFref>"
				If Len(aNfVinc[nX]:TEXT)<44
					cString += '<refNFe>'+aNfVinc[nX]:TEXT+Modulo11(aNfVinc[nX]:TEXT)+'</refNFe>'
				Else
					cString += '<refNFe>'+aNfVinc[nX]:TEXT+'</refNFe>'
				EndIf
				cString += "</NFref>"
			Next nX	
		EndIf
		If Type("oXml:_NFRef:_refNF")<>"U"
			If ValType(oXml:_NFRef:_refNF)=="A"
				aNfVinc := oXml:_NFRef:_refNF
			Else
				aNfVinc := {oXml:_NFRef:_refNF}
			EndIf
			For nX := 1 To Len(aNfVinc)				
				If aNfVinc[nX]:_Mod:TEXT == "55"
					If cVersao < "1.11"
						cNFREF := aNfVinc[nX]:_cUF:TEXT+aNfVinc[nX]:_AAMM:TEXT+aNfVinc[nX]:_CNPJ:TEXT+aNfVinc[nX]:_Mod:TEXT+StrZero(Val(aNfVinc[nX]:_Serie:TEXT),3)+StrZero(Val(aNfVinc[nX]:_nNF:TEXT),9)+StrZero(Val(aNfVinc[nX]:_cNF:TEXT),9)
					Else
						cNFREF := aNfVinc[nX]:_cUF:TEXT+aNfVinc[nX]:_AAMM:TEXT+aNfVinc[nX]:_CNPJ:TEXT+aNfVinc[nX]:_Mod:TEXT+StrZero(Val(aNfVinc[nX]:_Serie:TEXT),3)+StrZero(Val(aNfVinc[nX]:_nNF:TEXT),9)+nAutoriz+StrZero(Val(SubStr(aNfVinc[nX]:_cNF:TEXT,2,8)),8)
					EndIf
					cString += "<NFref>"
					cString += '<refNFe>'
					cString += cNFRef+Modulo11(cNFRef)
					cString += '</refNFe>'
					cString += "</NFref>"
				EndIf
			Next nX			
			For nX := 1 To Len(aNfVinc)
				If aNfVinc[nX]:_Mod:TEXT <> "55"
					cString += "<NFref>"
					cString += '<refNF>'
					cString += '<cUF>'  +aNfVinc[nX]:_cUF:TEXT+'</cUF>'
					cString += '<AAMM>' +aNfVinc[nX]:_AAMM:TEXT+'</AAMM>'
					cString += '<CNPJ>' +aNfVinc[nX]:_CNPJ:TEXT+'</CNPJ>'
					cString += '<mod>'  +aNfVinc[nX]:_Mod:TEXT+'</mod>'
					cString += '<serie>'+aNfVinc[nX]:_Serie:TEXT+'</serie>'
					cString += '<nNF>'  +aNfVinc[nX]:_nNF:TEXT+'</nNF>'
					cString += '</refNF>'
					cString += "</NFref>"
				EndIf
			Next nX			
		EndIf
		//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
		//³NOTA FISCAL DE PRODUTOR RURAL REFERENCIADA³
		//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
		
		If Type("oXml:_NFRef:_refNFP")<>"U" .And. cVersao >= "2.00"
//			aNfVinc := {}
			If ValType(oXml:_NFRef:_refNFP)=="A"
				oXmlVinc := oXml:_NFRef:_refNFP
			Else
				oXmlVinc := {oXml:_NFRef:_refNFP}
			EndIf
			For nX := 1 To Len(oXmlVinc)
				If oXmlVinc[nX]:_Mod:TEXT <> "55"
					cString += "<NFref>"
					cString += '<refNFP>'
					cString += '<cUF>'  +oXmlVinc[nX]:_cUF:TEXT+'</cUF>'
					cString += '<AAMM>' +oXmlVinc[nX]:_AAMM:TEXT+'</AAMM>'
					If Type("oXmlVinc[1]:_CNPJ:TEXT") <> "U"
						cString += '<CNPJ>' +oXmlVinc[nX]:_CNPJ:TEXT+'</CNPJ>'
					ElseIf Type("oXmlVinc[nX]:_CPF:TEXT") <> "U"
						cString += '<CPF>'  +oXmlVinc[nX]:_CPF:TEXT+'</CPF>'
					Else
						cString += '<CNPJ></CNPJ>'
					EndIF
					cString += '<IE>'  +oXmlVinc[nX]:_IE:TEXT+'</IE>'
					cString += '<mod>'  +oXmlVinc[nX]:_Mod:TEXT+'</mod>'
					cString += '<serie>'+oXmlVinc[nX]:_Serie:TEXT+'</serie>'
					cString += '<nNF>'  +oXmlVinc[nX]:_nNF:TEXT+'</nNF>'
					cString += '</refNFP>'
					cString += "</NFref>"
				EndIf
			Next nX
		EndIf
		//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
		//³CT-E EMITIDO ANTERIORMENTE REFERENCIADA   ³
		//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
		If Type("oXml:_NFRef:_refCTE:TEXT")<>"U" .AND. cVersao >= "2.00"
			aNfVinc := {}
			If ValType(oXml:_NFRef:_refCTE)=="A"
				aNfVinc := oXml:_NFRef:_refCTE
			Else
				aNfVinc := {oXml:_NFRef:_refCTE}            
			EndIf
			For nX := 1 To Len(aNfVinc)
				cString += "<NFref>"
				If Len(aNfVinc[nX]:TEXT)<44
					cString += '<refCTe>'+aNfVinc[nX]:TEXT+Modulo11(aNfVinc[nX]:TEXT)+'</refCTe>'
				Else
					cString += '<refCTe>'+aNfVinc[nX]:TEXT+'</refCTe>'
				EndIf
				cString += "</NFref>"
			Next nX
		EndIF
		
		
		//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
		//³INFORMACAO DO CUPOM FISCAL REFERENCIADO³
		//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
		
		If Type("oXml:_NFRef:_refECF")<>"U" .And. cVersao >= "2.00"
			aNfVinc := {}
			If ValType(oXml:_NFRef:_refECF)=="A"
				aNfVinc := oXml:_NFRef:_refECF
			Else
				aNfVinc := {oXml:_NFRef:_refECF}
			EndIf
			For nX := 1 To Len(aNfVinc)
				If aNfVinc[nX]:_Mod:TEXT <> "55"
					cString += "<NFref>"
					cString += '<refECF>'
					cString += '<mod>'  +aNfVinc[nX]:_Mod:TEXT+'</mod>'
					cString += '<nECF>' +aNfVinc[nX]:_nECF:TEXT+'</nECF>'
					cString += '<nCOO>' +aNfVinc[nX]:_nCOO:TEXT+'</nCOO>'
					cString += '</refECF>'
					cString += "</NFref>"
				EndIf
			Next nX
		EndIf
	EndIf
	If Type("oXml:_TpImp:TEXT")=="U"
		cString += '<tpImp>1</tpImp>'
	Else
		cString += '<tpImp>'+oIde:_TpImp:TEXT+'</tpImp>'
	EndIf
	Do Case
		Case cVersao <= "1.07"
			cString += '<tpEmis>'+cModal+'</tpEmis>'
		Case nModal == "2" .And. cVersao <= "2.00"
			cString += '<tpEmis>2</tpEmis>'
			lConting := .T.
		Case nModal == "3" .And. cVersao <= "2.00"
			cString += '<tpEmis>3</tpEmis>'
			lConting := .T.
		Case nModal == "4" .And. cVersao <= "2.00"
			cString += '<tpEmis>1</tpEmis>'
		Case nModal == "5"
			cString += '<tpEmis>4</tpEmis>'
			lConting := .T.
		Case nModal == "7" //"7-Contingência FS-DA"
			cString += '<tpEmis>5</tpEmis>'
			lConting := .T.
		OtherWise                
			cString += '<tpEmis>1</tpEmis>'  
	EndCase
	cString += '<cDV>'+Modulo11(cChave)+'</cDV>'
	If cVersao >= "1.10"
		cString += '<tpAmb>'+cAmbiente+'</tpAmb>'	
		cString += '<finNFe>'+oIde:_tpNFe:TEXT+'</finNFe>'
		cString += '<procEmi>0</procEmi>'
		cString += '<verProc>'+GetTssVersao()+'</verProc>'
		//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
		//³Novas Tags Versao 2.00 NF-e, <dhCont> e <xJust>³
		//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
		If lConting	 .And. cVersao >= "2.00"
			
			BeginSql Alias "QRYSPED056"
					Select MAX(CODCONT) AS CODCONT
					    FROM SPED056
						 WHERE ID_ENT = %Exp:cIdEnt%
			EndSql		  					         
			dbSelectArea("SPED056")
			dbSetorder(1)
			If dbSeek(cIdEnt+Str(QRYSPED056->CODCONT,15,0))			
		        cDhcont:= SpedDateConv(SPED056->DATE_INI,"YYYY")+"-"+SpedDateConv(SPED056->DATE_INI,"MM")+"-"+SpedDateConv(SPED056->DATE_INI,"DD")
		        cDhcont+= "T"+SPED056->TIME_INI
				cString += '<dhCont>'+cDhcont+'</dhCont>'
				If !EMPTY(SPED056->MOTIVO)
					cString += '<xJust>'+Alltrim(SPED056->MOTIVO)+'</xJust>'
				EndIf
			EndIf
			QRYSPED056->(dbCloseArea())	
		EndIf
	EndIf
	cString += '</ide>'
Else
	oXml := oIde
	cString += '<infNFe versao="T01.00">'
	cString += '<ide>'
	cString += '<cNF>'+oIde:_cNF:TEXT+'</cNF>'
	cString += '<natOp>'+oIde:_natOp:TEXT+'</natOp>'
	cString += '<indPag>'+oIde:_indPag:TEXT+'</indPag>'
	cString += '<Serie>'+oIde:_serie:TEXT+'</Serie>'
	cString += '<nNF>'+oIde:_nNF:TEXT+'</nNF>'
	cString += '<dEmi>'+oIde:_dEmi:TEXT+'</dEmi>
	cString += NfeTag('<dSaiEnt>',"oIde:_dSaiEnt:TEXT")
	cString += '<tpNF>'+oIde:_tpNF:TEXT+'</tpNF>'
	If Type("oXml:_cMunFG")<>"U"
		cString += '<cMunFG>'+oIde:_cMunFG:TEXT+'</cMunFG>'
	EndIf
	                        
	If Type("oXml:_NFRef")<>"U"
		cString += '<NFRef>'
		If Type("oXml:_NFRef:_refNFe")<>"U"
			If ValType(oXml:_NFRef:_refNFe)=="A"
				aNfVinc := oXml:_NFRef:_refNFe
			Else
				aNFVinc := {oXml:_NFRef:_refNFe}
			EndIf
			For nX := 1 To Len(aNFVinc)
				cString += '<refNFe>'+aNFVinc[nX]:TEXT+'</refNFe>'
			Next nX
		EndIf
		If Type("oXml:_NFRef:_RefNF")<>"U"
			If ValType(oXml:_NFRef:_refNF)=="A"
				aNfVinc := oXml:_NFRef:_refNF
			Else
				aNFVinc := {oXml:_NFRef:_refNF}
			EndIf
			cString += '<refNF>'
			For nX := 1 To Len(aNFVinc)
				cString += '<cUF>'  +aNfVinc[nX]:_cUF:TEXT+'</cUF>'
				cString += '<AAMM>' +aNfVinc[nX]:_AAMM:TEXT+'</AAMM>'
				cString += '<CNPJ>' +aNfVinc[nX]:_CNPJ:TEXT+'</CNPJ>'
				cString += '<mod>'  +aNfVinc[nX]:_Mod:TEXT+'</mod>'
				cString += '<Serie>'+aNfVinc[nX]:_Serie:TEXT+'</Serie>'
				cString += '<nNF>'  +aNfVinc[nX]:_nNF:TEXT+'</nNF>'
			Next nX
			cString += '</refNF>'
		EndIf
		cString += '</NFRef>'
	EndIf
	If Type("oXml:_TpAmb:TEXT")
		cString += '<tpAmb>'+oIde:_TpAmb:TEXT+'</tpAmb>'
	EndIf			
	If Type("oXml:_TpNFe:TEXT")
		cString += '<Tpnfe>'+oIde:_TpNFe:TEXT+'</Tpnfe>'
	ElseIf Type("oXml:_finNFe:TEXT")
		cString += '<Tpnfe>'+oIde:_finNFe:TEXT+'</Tpnfe>'
	EndIf
	cString += '</ide>'
EndIf
Return(cString)

Static Function XmlNfeEmit(cVersao,oEmit)

Private oXml    := oEmit
cString := ""

cString := '<emit>'
If Type("oXml:_CNPJ:TEXT")<>"U"
	cString += '<CNPJ>'+oEmit:_CNPJ:TEXT+'</CNPJ>
EndIf
If cVersao >= "1.10" .And. Type("oXml:_CPF:TEXT")<>"U"
	cString += '<CPF>' +oEmit:_CPF:TEXT+'</CPF>
EndIf
cString += '<xNome>'+oEmit:_Nome:TEXT+'</xNome>'
cString += NfeTag('<xFant>',"oXml:_Fant:TEXT")
cString += '<enderEmit>'
cString += '<xLgr>' +oEmit:_EnderEmit:_Lgr:TEXT+'</xLgr>'
If cVersao <= "1.10" .Or. Type("oXml:_EnderEmit:_Nro:TEXT")<>"U"
	cString += NfeTag('<nro>',"oXml:_EnderEmit:_Nro:TEXT",.T.)
Else
	cString += '<nro>s/n</nro>'
EndIf
cString += NfeTag('<xCpl>',"oXml:_EnderEmit:_Cpl:TEXT")
cString += '<xBairro>'+oEmit:_EnderEmit:_Bairro:TEXT+'</xBairro>'
cString += '<cMun>'   +oEmit:_EnderEmit:_cMun:TEXT+'</cMun>'
cString += '<xMun>'   +oEmit:_EnderEmit:_Mun:TEXT+'</xMun>'
cString += '<UF>'     +oEmit:_EnderEmit:_UF:TEXT+'</UF>'
cString += NfeTag('<CEP>',"oXml:_EnderEmit:_Cep:TEXT")
cString += NfeTag('<cPais>',"oXml:_EnderEmit:_cPais:TEXT")
cString += NfeTag('<xPais>',"oXml:_EnderEmit:_Pais:TEXT")
If cVersao < "1.11"
	cString += NfeTag('<fone>',"ConvType(oXml:_EnderEmit:_Fone:TEXT,10,0)")
Else
	cString += NfeTag('<fone>',"ConvType(oXml:_EnderEmit:_Fone:TEXT,14,0)")
EndIf
cString += '</enderEmit>'
cString += '<IE>'+oEmit:_IE:TEXT+'</IE>'
cString += NfeTag('<IEST>',"oXml:_IEST:TEXT",.F.)
If cVersao >= "1.10"
	cString += NfeTag('<IM>'  ,"oXml:_IM:TEXT")
	cString += NfeTag('<CNAE>',"IIF(!Empty(oXml:_IM:TEXT),oXml:_CNAE:TEXT,'')")
	IF cVersao >= "2.00"
		cString += '<CRT>'+oEmit:_CRT:TEXT+'</CRT>'
	EndIf
Else
	cString += NfeTag('<IM>'  ,"oXml:_IM:TEXT")
EndIf
cString += '</emit>'
Return(cString)

Static Function XmlNfeDest(cVersao,oDest,cMail)

Private oXml    := oDest
cString := ""
cString := '<dest>'
If Type("oXml:_CNPJ:TEXT")<>"U"
	cString += '<CNPJ>'+oDest:_CNPJ:TEXT+'</CNPJ>
ElseIf Type("oXml:_CPF:TEXT")<>"U"
	cString += NfeTag('<CPF>' ,"oXml:_CPF:TEXT")
Else
	cString += '<CNPJ></CNPJ>'
EndIf
cString += '<xNome>'+oDest:_Nome:TEXT+'</xNome>'
cString += '<enderDest>'
cString += '<xLgr>'+oDest:_EnderDest:_Lgr:TEXT+'</xLgr>'
cString += NfeTag('<nro>',"oXml:_EnderDest:_nro:TEXT",.T.)
cString += NfeTag('<xCpl>',"oXml:_EnderDest:_Cpl:TEXT")
cString += '<xBairro>'+oDest:_EnderDest:_Bairro:TEXT+'</xBairro>'
cString += '<cMun>'+oDest:_EnderDest:_cMun:TEXT+'</cMun>'
cString += '<xMun>'+oDest:_EnderDest:_Mun:TEXT+'</xMun>'
cString += '<UF>'+oDest:_EnderDest:_UF:TEXT+'</UF>'
cString += NfeTag('<CEP>',"oXml:_EnderDest:_CEP:TEXT")
cString += NfeTag('<cPais>',"oXml:_EnderDest:_cPais:TEXT")
cString += NfeTag('<xPais>',"oXml:_EnderDest:_Pais:TEXT")
If cVersao < "1.11"
	cString += NfeTag('<fone>',"ConvType(oXml:_EnderDest:_fone:TEXT,10,0)")
Else
	cString += NfeTag('<fone>',"ConvType(oXml:_EnderDest:_fone:TEXT,14,0)")
EndIf
cString += '</enderDest>'
cString += '<IE>'+oDest:_IE:TEXT+'</IE>'
cString += NfeTag('<ISUF>',"oXml:_IESUF:TEXT")
If Type("oXml:_eMail:TEXT")<>"U"
	cMail := oXml:_eMail:TEXT
	If cVersao>="2.00"	              
		cString += '<email>'+oXml:_eMail:TEXT+'</email>'	
	EndIf
EndIf
cString += '</dest>'
Return(cString)

Static Function XmlNfeRetirada(cVersao,oRetira)

Private oXml    := oRetira
cString := ""
If oRetira <> Nil
	cString := '<retirada>'
	If Type("oXml:_CNPJ:TEXT")<>"U"
		cString += '<CNPJ>'+oXml:_CNPJ:TEXT+'</CNPJ>
	ElseIf Type("oXml:_CPF:TEXT")<>"U"
		cString += NfeTag('<CPF>' ,"oXml:_CPF:TEXT")
	Else
		cString += '<CNPJ></CNPJ>'
	EndIf
	cString += '<xLgr>'+oRetira:_Lgr:TEXT+'</xLgr>'
	If cVersao <= "1.10" .Or. Type("oXml:_nro:TEXT")<>"U"
		cString += NfeTag('<nro>',"oXml:_nro:TEXT",.T.)
	Else
		cString += '<nro>s/n</nro>'
	EndIf
	cString += NfeTag('<xCpl>',"oXml:_Cpl:TEXT")
	cString += '<xBairro>'+oRetira:_Bairro:TEXT+'</xBairro>'
	cString += '<cMun>'+oRetira:_cMun:TEXT+'</cMun>'
	cString += '<xMun>'+oRetira:_Mun:TEXT+'</xMun>'
	cString += '<UF>'+oRetira:_UF:TEXT+'</UF>'
	cString += '</retirada>'
EndIf
Return(cString)

Static Function XmlNfeEntrega(cVersao,oEntrega)

Private oXml    := oEntrega
cString := ""
If oEntrega <> Nil
	cString := '<entrega>'
	If Type("oXML:_CNPJ:TEXT")<>"U"
		cString += '<CNPJ>'+oXml:_CNPJ:TEXT+'</CNPJ>
	ElseIf Type("oXml:_CPF:TEXT")<>"U" .And. cVersao >= "2.00"
		cString += NfeTag('<CPF>' ,"oEntrega:_CPF:TEXT")
	Else
		cString += '<CNPJ></CNPJ>'
	EndIf	
	cString += '<xLgr>'+oXML:_Lgr:TEXT+'</xLgr>'
	If cVersao <= "1.10" .Or. Type("oXml:_nro:TEXT")<>"U"
		cString += NfeTag('<nro>',"oXml:_nro:TEXT",.T.)
	Else
		cString += '<nro>s/n</nro>'
	EndIf	
	cString += NfeTag('<xCpl>',"oXml:_Cpl:TEXT")
	cString += '<xBairro>'+oEntrega:_Bairro:TEXT+'</xBairro>'
	cString += '<cMun>'+oEntrega:_cMun:TEXT+'</cMun>'
	cString += '<xMun>'+oEntrega:_Mun:TEXT+'</xMun>'
	cString += '<UF>'+oEntrega:_UF:TEXT+'</UF>'
	cString += '</entrega>'
EndIf
Return(cString)

Static Function XmlNfeItem(cVersao,oDet,aImp,aTot,lCdata)

Local nDI        := 0
Local nadi       := 0
Local nMed		 := 0
Local nArma		 := 0
Local nveicProd	 := 0
Local cGrupo     := ""
Local nValPis    := 0
Local nValCof    := 0
Local lICMSComb  := .F.
Local lPis       := .F.
Local lCofins    := .F. 
Default lCdata	 := .F.
Private nX       := 0
Private nY       := 0
Private oXml     := oDet
Private aImposto := {}
Private aadi     := {}
Private aDI      := {}
Private aMed     := {}
Private aArma    := {}
Private aveicProd:= {}
Private cEAN     := ""
Private cString  := ""
cString += '<det nItem="'+oDet:_nItem:TEXT+'">'
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Monta a tag de produtos                                                 ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
cString += '<prod>'
cString += '<cProd>' +oDet:_Prod:_cprod:TEXT+'</cProd>'

cEAN:= AllTrim(oDet:_Prod:_EAN:TEXT)
nX := Len(cEAN)
cEAN := Val(cEAN)
cEAN := AllTrim(Str(cEAN,nX))
nX := Len(cEAN)
If nX <> 8 .And. nX <> 12 .And. nX <> 13 .And. nX <> 14
	cEAN := ""
EndIf
cString += '<cEAN>'  +cEAN+'</cEAN>'
cString += '<xProd>' + oDet:_Prod:_prod:TEXT  + oDet:_infAdProd:TEXT  +'</xProd>'  // Roberto Fiuza informacoes complemntar FCI
cString += NfeTag('<NCM>',"oXml:_Prod:_ncm:TEXT")
cString += NfeTag('<EXTIPI>',"oXml:_Prod:_extipi:TEXT")
If cVersao < "2.00"
	cString += NfeTag('<genero>',"SubStr(IIf(type('oXml:_Prod:_ncm:TEXT')=='U','',oXml:_Prod:_ncm:TEXT),1,2)")
EndIf
cString += '<CFOP>'  +oDet:_Prod:_cfop:TEXT+'</CFOP>'
If cVersao == "1.07"
	cString += '<uTrib>' +oDet:_Prod:_uTrib:TEXT+'</uTrib>'
EndIf
cString += NfeTag('<uCom>',"oXml:_Prod:_uCom:TEXT")
If cVersao < "1.10"
	cString += '<qTrib>' +Convtype(Val(oXml:_Prod:_qTrib:TEXT),11,3)+'</qTrib>'
	If Type("oXml:_Prod:_qCom:TEXT") <> "U"
		cString += NfeTag('<qCom>',"Convtype(Val(oXml:_Prod:_qCom:TEXT),11,3)",.T.)
	Else
		cString += "<qCom>0.000</qCom>"
	EndIf
ElseIf cVersao >= "2.00"
	If Type("oXml:_Prod:_qCom:TEXT") <> "U"
		cString += NfeTag('<qCom>',"Convtype(Val(oXml:_Prod:_qCom:TEXT),15,4)",.T.)
	Else
		cString += "<qCom>0.0000</qCom>"
	EndIf
Else
	If Type("oXml:_Prod:_qCom:TEXT") <> "U"
		cString += NfeTag('<qCom>',"Convtype(Val(oXml:_Prod:_qCom:TEXT),12,4)",.T.)
	Else
		cString += "<qCom>0.0000</qCom>"
	EndIf
EndIf
If cVersao >= "2.00"
	If Type("oXml:_Prod:_vUnCom:TEXT") <> "U"
		cString += NfeTag('<vUnCom>',"Convtype(Val(oXml:_Prod:_vUnCom:TEXT),21,10)",.T.)
	Else
		cString += "<vUnCom>0.0000</vUnCom>"
	EndIf
ElseIf cVersao <= "1.10"
	If Type("oXml:_Prod:_vUnCom:TEXT") <> "U"
		cString += NfeTag('<vUnCom>',"Convtype(Val(oXml:_Prod:_vUnCom:TEXT),16,4)",.T.)
	Else
		cString += "<vUnCom>0.0000</vUnCom>"
	EndIf
EndIf
cString += '<vProd>' +ConvType(Val(oDet:_Prod:_vProd:TEXT),15,2)+'</vProd>'
If cVersao >= "1.10"
	If Type("oXml:_Prod:_cEANTrib:TEXT")<>"U"	
		cEAN:= AllTrim(oXml:_Prod:_cEANTrib:TEXT)
		nX := Len(cEAN)
		cEAN := Val(cEAN)
		cEAN := AllTrim(Str(cEAN,nX))
		nX := Len(cEAN)
		If nX <> 8 .And. nX <> 12 .And. nX <> 13 .And. nX <> 14
			cEAN := ""
		EndIf
	EndIf
	cString += NfeTag('<cEANTrib>',"cEAN",.T.)
	cString += '<uTrib>' +oDet:_Prod:_uTrib:TEXT+'</uTrib>'
	If cVersao <= "1.10"
		cString += '<qTrib>' +ConvType(Val(oDet:_Prod:_qTrib:TEXT),12,4)+'</qTrib>'
		cString += NfeTag('<vUnTrib>',"ConvType(Val(oXml:_Prod:_vUnTrib:TEXT),16,4)",.T.)
	ElseIf cVersao >= "2.00"
		cString += '<qTrib>' +ConvType(Val(oDet:_Prod:_qTrib:TEXT),15,4)+'</qTrib>'
		cString += NfeTag('<vUnTrib>',"ConvType(Val(oXml:_Prod:_vUnTrib:TEXT),21,10)",.T.)
	EndIf
EndIf                
              
//Roberto Fiuza 17/01/16 nao existe mais a tags abaixo

//cString += NfeTag('<vFrete>',"ConvType(Val(oXml:_Prod:_vFrete:TEXT),15,2)")
//cString += NfeTag('<vSeg>'  ,"ConvType(Val(oXml:_Prod:_vSeg:TEXT),15,2)")
//cString += NfeTag('<vDesc>' ,"ConvType(Val(oXml:_Prod:_vDesc:TEXT),15,2)")
//If cVersao >= "2.00"
//	cString += NfeTag('<vOutro>' ,"ConvType(Val(oXml:_Prod:_vOutro:TEXT),15,2)")
//	cString += '<indTot>'+oDet:_Prod:_indTot:TEXT+'</indTot>'
//EndIF


//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Monta a tag de DI                                                       ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
If Type("oXml:_Prod:_DI")<>"U" 
	If ValType(oXml:_Prod:_DI)=="A"
		aDI := oXml:_Prod:_DI
	Else
		aDI := {oXml:_Prod:_DI}
	EndIf
	If ValType(oXml:_Prod:_DI:_adicao)=="A"
		aAdi := oXml:_Prod:_DI:_adicao
	Else
		aAdi := {oXml:_Prod:_DI:_adicao}
	EndIf
	For nDi := 1 To Len(aDI)	
		cString += '<DI>'
		cString += '<nDI>' +aDI[nDI]:_ndi:TEXT+'</nDI>'
		cString += '<dDI>' +aDI[nDI]:_dtdi:TEXT+'</dDI>'
		cString += '<xLocDesemb>' +aDI[nDI]:_LocDesemb:TEXT+'</xLocDesemb>'
		cString += '<UFDesemb>' +aDI[nDI]:_UFDesemb:TEXT+'</UFDesemb>'
		cString += '<dDesemb>' +aDI[nDI]:_dtDesemb:TEXT+'</dDesemb>'
		cString += '<cExportador>' +aDI[nDI]:_Exportador:TEXT+'</cExportador>'
		For nAdi := 1 To Len(aAdi)
			cString += '<adi>'
			cString += '<nAdicao>' +aAdi[nAdi]:_Adicao:TEXT+'</nAdicao>'
			cString += '<nSeqAdic>' +aAdi[nAdi]:_SeqAdic:TEXT+'</nSeqAdic>'
			cString += '<cFabricante>' +aAdi[nAdi]:_Fabricante:TEXT+'</cFabricante>'
			cString += NfeTag('<vDescDI>' ,"ConvType(Val(aAdi[nAdi]:_vDescDI:TEXT),15,2)")
			cString += '</adi>'
		Next nAdi
		cString += '</DI>'
	Next nDi
EndIf 
IF cVersao >= "2.00"
	If Type("oXml:_Prod:_xPed:TEXT")<>"U"
		cString+= '<xPed>'+oXml:_Prod:_xPed:TEXT+'</xPed>'
	EndIf
	If Type("oXml:_Prod:_nItemPed:TEXT")<>"U"
		cString+= '<nItemPed>'+oXml:_Prod:_nItemPed:TEXT+'</nItemPed>'
	EndIf
EndIf
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Monta da tag de Veiculos Novos                                          ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
If cVersao >= "1.10" .And. Type("oXml:_Prod:_veicProd")<>"U" 
	If 	ValType(oXml:_Prod:_veicProd)=="A"
		aveicProd := oXml:_Prod:_veicProd
	Else
		aveicProd := {oXml:_Prod:_veicProd}
	EndIf
	For nveicProd := 1 To Len(aveicProd)
		nY := nveicProd
		cString += '<veicProd>'
		cString += NfeTag('<tpOp>'   ,"convtype(aVeicProd[nY]:_tpOp:TEXT,1)"   ,.T.)
		cString += NfeTag('<chassi>' ,"convtype(aVeicProd[nY]:_chassi:TEXT,17)",.T.)
		cString += NfeTag('<cCor>'   ,"convtype(aVeicProd[nY]:_cCor:TEXT,4)"   ,.T.)
		cString += NfeTag('<xCor>'   ,"convtype(aVeicProd[nY]:_xCor:TEXT,40)"  ,.T.)
		cString += NfeTag('<pot>'    ,"convtype(aVeicProd[nY]:_pot:TEXT,4)"    ,.T.)
		If cVersao >= "2.00"
			cString += NfeTag('<cilin>'    ,"convtype(aVeicProd[nY]:_cilin:TEXT,4)"    ,.T.)
		Else
			cString += NfeTag('<CM3>'    ,"convtype(aVeicProd[nY]:_cm3:TEXT,4)"    ,.T.)
		EndIf
		cString += NfeTag('<pesoL>'  ,"convtype(aVeicProd[nY]:_pesol:TEXT,9)"  ,.T.)
		cString += NfeTag('<pesoB>'  ,"convtype(aVeicProd[nY]:_pesob:TEXT,9)"  ,.T.)
		cString += NfeTag('<nSerie>' ,"convtype(aVeicProd[nY]:_nserie:TEXT,9)" ,.T.)
		If cVersao >= "2.00"
			cString += NfeTag('<tpComb>' ,"convtype(aVeicProd[nY]:_tpcomb:TEXT,2)" ,.T.)
		Else
			cString += NfeTag('<tpComb>' ,"convtype(aVeicProd[nY]:_tpcomb:TEXT,8)" ,.T.)
		EndIf
		cString += NfeTag('<nMotor>' ,"convtype(aVeicProd[nY]:_nmotor:TEXT,21)",.T.)
		If cVersao >= "2.00"
			cString += NfeTag('<CMT>'   ,"convtype(aVeicProd[nY]:_CMT:TEXT,9)"   ,.T.)
		Else
			cString += NfeTag('<CMKG>'   ,"convtype(aVeicProd[nY]:_cmkg:TEXT,9)"   ,.T.)
		EndIf
		cString += NfeTag('<dist>'   ,"convtype(aVeicProd[nY]:_dist:TEXT,4)"   ,.T.)
		If Type("aVeicProd[nY]:_renavam")<>"U"
			cString += NfeTag('<RENAVAM>',"convtype(aVeicProd[nY]:_renavam:TEXT,9)",.T.)
		EndIf
		cString += NfeTag('<anoMod>' ,"convtype(aVeicProd[nY]:_anomod:TEXT,4)" ,.T.)
		cString += NfeTag('<anoFab>' ,"convtype(aVeicProd[nY]:_anofab:TEXT,4)" ,.T.)
		cString += NfeTag('<tpPint>' ,"convtype(aVeicProd[nY]:_tppint:TEXT,1)" ,.T.)
		cString += NfeTag('<tpVeic>' ,"convtype(aVeicProd[nY]:_tpveic:TEXT,2)" ,.T.)
		cString += NfeTag('<espVeic>',"convtype(aVeicProd[nY]:_espvei:TEXT,1)" ,.T.)
		cString += NfeTag('<VIN>'    ,"convtype(aVeicProd[nY]:_vin:TEXT,1)"    ,.T.)
		cString += NfeTag('<condVeic>',"convtype(aVeicProd[nY]:_condvei:TEXT,1)",.T.)
		cString += NfeTag('<cMod>'   ,"convtype(aVeicProd[nY]:_cmod:TEXT,6)"   ,.T.)
		If cVersao >= "2.00"
			cString += NfeTag('<cCorDENATRAN>'   ,"convtype(aVeicProd[nY]:_cCorDENATRAN:TEXT,2)"   ,.T.)
			cString += '<lota>'+aVeicProd[nY]:_lota:TEXT+'</lota>'
			cString += NfeTag('<tpRest>'   ,"convtype(aVeicProd[nY]:_tpRest:TEXT,1)"   ,.T.)
		EndIf
		cString += '</veicProd>'
	Next nveicProd
EndIf 
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Monta da tag de medicamentos                                            ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
If cVersao >= "1.10" .And. Type("oXml:_Prod:_med")<>"U" 
	If 	ValType(oXml:_Prod:_med)=="A"
		aMed := oXml:_Prod:_med
	Else
		aMed := {oXml:_Prod:_med}
	EndIf
	For nMed := 1 To Len(aMed)
		nY := nMed
		cString += '<med>'
		cString += NfeTag('<nLote>',"convtype(aMed[nY]:_lote:TEXT,20)",.T.)
		cString += NfeTag('<qLote>',"convtype(val(aMed[nY]:_qlote:TEXT),11,3)",.T.)	
		cString += NfeTag('<dFab>' ,"convtype(aMed[nY]:_dtfab:TEXT)",.T.)	
		cString += NfeTag('<dVal>' ,"convtype(aMed[nY]:_dtval:TEXT)",.T.)		
		cString += NfeTag('<vPMC>' ,"convtype(val(aMed[nY]:_vpmc:TEXT),15,2)",.T.)
		cString += '</med>'
	Next nMed
EndIf 
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Monta da tag de armamentos                                            ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
If cVersao >= "1.10" .And. Type("oXml:_Prod:_arma")<>"U" 
	If 	ValType(oXml:_Prod:_arma)=="A"
		aArma := oXml:_Prod:_arma
	Else
		aArma := {oXml:_Prod:_arma}
	EndIf
	For nArma := 1 To Len(aArma)
		nY := nArma
		cString += '<arma>'
		cString += NfeTag('<tpArma>',"convtype(aArma[nY]:_tpArma:TEXT)",.T.)
		cString += NfeTag('<nSerie>',"convtype(aArma[nY]:_nSerie:TEXT)",.T.)	
		cString += NfeTag('<nCano>' ,"convtype(aArma[nY]:_nCano:TEXT)",.T.)	
		cString += NfeTag('<descr>' ,"convtype(aArma[nY]:_descr:TEXT)",.T.)		
		cString += '</arma>'
	Next nArma
EndIf  
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Monta da tag de combustiveis                                            ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
If cVersao >= "1.10" .And. Type("oXml:_Prod:_comb")<>"U" 
	cString += '<comb>'
	cString += NfeTag('<cProdANP>',"oXml:_Prod:_comb:_cProdANP:TEXT")
	cString += NfeTag('<CODIF>',"oXml:_Prod:_comb:_CODIF:TEXT")	
	cString += NfeTag('<qTemp>',"oXml:_Prod:_comb:_qTemp:TEXT")
	cString += '<UFCons>'+oXml:_Prod:_comb:_UFCons:TEXT+'</UFCons>'
	If Type("oXml:_Prod:_comb:_CIDE")<>"U" 
		cString += '<CIDE>'
		cString += '<qBCprod>' +ConvType(Val(oXml:_Prod:_comb:_CIDE:_qBCProd:TEXT),16,4)+'</qBCprod>'
		cString += '<vAliqProd>'+ConvType(Val(oXml:_Prod:_comb:_CIDE:_vAliqProd:TEXT),15,4)+'</vAliqProd>'
		cString += '<vCIDE>'+ConvType(Val(oXml:_Prod:_comb:_CIDE:_vCIDE:TEXT),15,2)+'</vCIDE>'
		cString += '</CIDE>'
	EndIf
	If cVersao < "2.00"
		cString += '<ICMSComb>'
		If ValType(oXml:_Imposto)=="A"
			aImposto := oXml:_Imposto
		Else
			aImposto := {oXml:_Imposto}
		EndIf
		nX := Ascan(aImposto,{|o| o:_Codigo:TEXT == "ICMS"})
		If nX > 0
			lICMSComb := .T.
			cString += '<vBCICMS>'+ConvType(Val(aImposto[nX]:_Tributo:_vBC:TEXT),15,2)+'</vBCICMS>'
			cString += '<vICMS>'  +ConvType(Val(aImposto[nX]:_Tributo:_Valor:TEXT),15,2)+'</vICMS>'
			nY := aScan(aImposto,{|x| x:_codigo:TEXT == "ICMSST" .And. IIf(Type("x:_Tributo:_CST:TEXT")<>"U",x:_Tributo:_CST:TEXT<>"60",.T.)})
			If nY <> 0
				cString += '<vBCICMSST>'+ConvType(Val(aImposto[nY]:_Tributo:_vBC:TEXT),15,2)+'</vBCICMSST>'
				cString += '<vICMSST>'+ConvType(Val(aImposto[nY]:_Tributo:_valor:TEXT),15,2)+'</vICMSST>'
			Else
				cString += '<vBCICMSST>0</vBCICMSST>'
				cString += '<vICMSST>0</vICMSST>'
			EndIf
		EndIf
		If !lICMSComb
			cString += '<vBCICMS>0</vBCICMS>'
			cString += '<vICMS>0</vICMS>'		
			cString += '<vBCICMSST>0</vBCICMSST>'
			cString += '<vICMSST>0</vICMSST>'		
		EndIf
		cString += '</ICMSComb>'
		If Type("oXml:_Prod:_comb:_ICMSInter")<>"U" 
			cString += '<ICMSInter>'
			cString += '<vBCICMSSTDest>' +ConvType(Val(oXml:_Prod:_comb:_ICMSInter:_vBCICMSSTDest:TEXT),15,2)+'</vBCICMSSTDest>'
			cString += '<vICMSSTDest>'   +ConvType(Val(oXml:_Prod:_comb:_ICMSInter:_vICMSSTDest:TEXT),15,2)+  '</vICMSSTDest>'
			cString += '</ICMSInter>'
		EndIf
		If Type("oXml:_Prod:_comb:_ICMSCons")<>"U" 
			cString += '<ICMSCons>'
			cString += '<vBCICMSSTCons>' +ConvType(Val(oXml:_Prod:_comb:_ICMSInter:_vBCICMSSTDest:TEXT),15,2)+'</vBCICMSSTCons>'
			cString += '<vICMSSTCons>'   +ConvType(Val(oXml:_Prod:_comb:_ICMSInter:_vICMSSTDest:TEXT),15,2)+  '</vICMSSTCons>'
			cString += '</ICMSCons>'
		EndIf
	EndIf
	cString += '</comb>'
EndIf
cString += '</prod>'
aTot[1] += Val(oDet:_Prod:_vProd:TEXT)
aTot[2] += Val(IIf(Type("oXml:_Prod:_vFrete:TEXT")=="U","0",oDet:_Prod:_vFrete:TEXT))
aTot[3] += Val(IIf(Type("oXml:_Prod:_vSeg:TEXT")  =="U","0",oDet:_Prod:_vSeg:TEXT))
aTot[4] += Val(IIf(Type("oXml:_Prod:_vDesc:TEXT") =="U","0",oDet:_Prod:_vDesc:TEXT))
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Monta a tag de impostos                                                 ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
cString += '<imposto>'
If ValType(oXml:_Imposto)=="A"
	aImposto := oXml:_Imposto
Else
	aImposto := {oXml:_Imposto}
EndIf
nX := Ascan(aImposto,{|o| o:_Codigo:TEXT == "ICMS"})
If nX > 0
	nY := aScan(aImposto,{|x| x:_codigo:TEXT == "ICMSST"})
	cGrupo  := aImposto[nX]:_Tributo:_CST:TEXT
	If cGrupo $ "40,41,50" .Or. (cGrupo == "51" .And. cVersao<="1.07")
		cGrupo := "40"
	EndIf
	cString += '<ICMS>'
	cString += '<ICMS'    +cGrupo+'>'
	cString += '<orig>'   +aImposto[nX]:_Cpl:_orig:TEXT+'</orig>'
	cString += '<CST>'    +aImposto[nX]:_Tributo:_CST:TEXT+'</CST>'
	If aImposto[nX]:_Tributo:_CST:TEXT$"00,10,20,70,90" .Or. (aImposto[nX]:_Tributo:_CST:TEXT == "51" .And. cVersao>="1.08")
		cString += '<modBC>'  +aImposto[nX]:_Tributo:_MODBC:TEXT+'</modBC>'
	EndIf
	If aImposto[nX]:_Tributo:_CST:TEXT$"20,70,90" .Or. (aImposto[nX]:_Tributo:_CST:TEXT == "51" .And. cVersao>="1.08")
		cString += NfeTag('<pRedBC>',"ConvType(Val(aImposto[nX]:_Tributo:_PREDBC:TEXT),5,2)")
	EndIf
	If aImposto[nX]:_Tributo:_CST:TEXT$"00,10,20,70,90" .Or. (aImposto[nX]:_Tributo:_CST:TEXT == "51" .And. cVersao>="1.08")
		cString += '<vBC>'    +ConvType(Val(aImposto[nX]:_Tributo:_vBC:TEXT),15,2)+'</vBC>'
		cString += '<pICMS>'  +ConvType(Val(aImposto[nX]:_Tributo:_Aliquota:TEXT),5,2)+'</pICMS>'
		cString += '<vICMS>'  +ConvType(Val(aImposto[nX]:_Tributo:_Valor:TEXT),15,2)+'</vICMS>'
		
		aImp[1][1] += Val(IIf(Type("aImposto[nX]:_Tributo:_vBC:TEXT")  =="U","0",aImposto[nX]:_Tributo:_vBC:TEXT))
		aImp[1][2] += Val(IIf(Type("aImposto[nX]:_Tributo:_valor:TEXT")=="U","0",aImposto[nX]:_Tributo:_valor:TEXT))		
	EndIf
	If aImposto[nX]:_Tributo:_CST:TEXT$"40,41,50" .And. cVersao >= "2.00"
		cString += NfeTag('<vICMS>' ,"ConvType(Val(aImposto[nX]:_Tributo:_Valor:TEXT),15,2)")					
		cString += NfeTag('<motDesICMS>' ,"aImposto[nX]:_Tributo:_motDesICMS:TEXT")
	EndIf
	If aImposto[nX]:_Tributo:_CST:TEXT$"10,30,70,90" .And. nY > 0
		cString += '<modBCST>'+aImposto[nY]:_Tributo:_MODBC:TEXT+'</modBCST>'
		cString += NfeTag('<pMVAST>'  ,"ConvType(Val(aImposto[nY]:_Cpl:_PMVAST:TEXT),5,2)")
		cString += NfeTag('<pRedBCST>',"ConvType(Val(aImposto[nY]:_Tributo:_PREDBC:TEXT),5,2)")
	EndIf
	If aImposto[nX]:_Tributo:_CST:TEXT$"10,30,60,70,90" .And. nY > 0
        If aImposto[nX]:_Tributo:_CST:TEXT$"60" .And. cVersao >= "2.00"
        	cString += '<vBCSTRet>'  +ConvType(Val(aImposto[nY]:_Tributo:_vBC:TEXT),15,2)+'</vBCSTRet>'
        Else	
			cString += '<vBCST>'  +ConvType(Val(aImposto[nY]:_Tributo:_vBC:TEXT),15,2)+'</vBCST>'
		EndIf	
	EndIf
	If aImposto[nX]:_Tributo:_CST:TEXT$"10,30,70,90" .And. nY > 0	
		cString += '<pICMSST>'+ConvType(Val(aImposto[nY]:_Tributo:_Aliquota:TEXT),5,2)+'</pICMSST>'

		aImp[2][1] += Val(IIf(Type("aImposto[nY]:_Tributo:_vBC:TEXT")  =="U","0",aImposto[nY]:_Tributo:_vBC:TEXT))
		aImp[2][2] += Val(IIf(Type("aImposto[nY]:_Tributo:_valor:TEXT")=="U","0",aImposto[nY]:_Tributo:_valor:TEXT))		
	EndIf
	If aImposto[nX]:_Tributo:_CST:TEXT$"10,30,60,70,90" .And. nY > 0
		If aImposto[nX]:_Tributo:_CST:TEXT$"60" .And. cVersao >= "2.00"
			cString += '<vICMSSTRet>'+ConvType(Val(aImposto[nY]:_Tributo:_valor:TEXT),15,2)+'</vICMSSTRet>'			
		Else
			cString += '<vICMSST>'+ConvType(Val(aImposto[nY]:_Tributo:_valor:TEXT),15,2)+'</vICMSST>'
		EndIf	
	EndIf
	cString += '</ICMS'+cGrupo+'>'
	cString += '</ICMS>'
EndIf                          
If cVersao >= "2.00"
	nX := aScan(aImposto,{|x| x:_codigo:TEXT == "ICMSPART"})
	If  nX> 0 .And. aImposto[nX]:_Tributo:_CST:TEXT$"10,90"
		cString += '<ICMS>'
		cString += '<ICMSPart>'
		cString += '<orig>'   +aImposto[nX]:_Cpl:_orig:TEXT+'</orig>'
		cString += '<CST>'    +aImposto[nX]:_Tributo:_CST:TEXT+'</CST>'
		cString += '<modBC>'  +aImposto[nX]:_Tributo:_MODBC:TEXT+'</modBC>'               
		cString += '<vBC>'    +ConvType(Val(aImposto[nX]:_Tributo:_vBC:TEXT),15,2)+'</vBC>'		
		cString += NfeTag('<pRedBC>',"ConvType(Val(aImposto[nX]:_Tributo:_PREDBC:TEXT),5,2)")
		cString += '<pICMS>'  +ConvType(Val(aImposto[nX]:_Tributo:_Aliquota:TEXT),5,2)+'</pICMS>'
		cString += '<vICMS>'  +ConvType(Val(aImposto[nX]:_Tributo:_Valor:TEXT),15,2)+'</vICMS>'
		cString += '<modBCST>'+aImposto[nX]:_Tributo:_MODBCST:TEXT+'</modBCST>'
		cString += NfeTag('<pMVAST>'  ,"ConvType(Val(aImposto[nX]:_Cpl:_PMVAST:TEXT),5,2)")
		cString += NfeTag('<pRedBCST>',"ConvType(Val(aImposto[nX]:_Tributo:_PREDBCST:TEXT),5,2)")
		cString += '<vBCST>'  +ConvType(Val(aImposto[nX]:_Tributo:_vBCST:TEXT),15,2)+'</vBCST>'
		cString += '<pICMSST>'+ConvType(Val(aImposto[nX]:_Tributo:_AliquotaST:TEXT),5,2)+'</pICMSST>'
		cString += '<vICMSST>'+ConvType(Val(aImposto[nX]:_Tributo:_valorST:TEXT),15,2)+'</vICMSST>'
		cString += '<pBCOp>'+aImposto[nX]:_Tributo:_pBCOp:TEXT+'</pBCOp>'				
		cString += '<UFST>'	+aImposto[nX]:_Tributo:_UFST:TEXT+'</UFST>'						
		cString += '</ICMSPart>'
		cString += '</ICMS>'
	EndIF
	
	nX := aScan(aImposto,{|x| x:_codigo:TEXT == "ICMSST41"})
	If  nX > 0 .And. aImposto[nX]:_Tributo:_CST:TEXT$"41"
		cString += '<ICMS>'	 
		cString += '<ICMSST>'
		cString += '<orig>'   +aImposto[nX]:_Cpl:_orig:TEXT+'</orig>'
		cString += '<CST>'    +aImposto[nX]:_Tributo:_CST:TEXT+'</CST>'  
		cString += '<vBCSTRet>'+ConvType(Val(aImposto[nX]:_Tributo:_vBC:TEXT),15,2)+'</vBCSTRet>'
		cString += '<vICMSSTRet>'+ConvType(Val(aImposto[nX]:_Tributo:_valor:TEXT),15,2)+'</vICMSSTRet>'
		cString += '<vBCSTDest>'+ConvType(Val(aImposto[nX]:_Tributo:_vBCSTDest:TEXT),15,2)+'</vBCSTDest>'
		cString += '<vICMSSTDest>'+ConvType(Val(aImposto[nX]:_Tributo:_vICMSSTDest:TEXT),15,2)+'</vICMSSTDest>'					
		cString += '</ICMSST>'
		cString += '</ICMS>'
	EndIF
	nX := Ascan(aImposto,{|o| o:_Codigo:TEXT == "ICMSSN"})
	If nX > 0
		cGrupo  := aImposto[nX]:_Tributo:_CSOSN:TEXT
		If cGrupo $ "102,103,300,400"
			cGrupo := "102"
		ElseIf cGrupo $ "202,203"
			cGrupo := "201"	
		EndIf      
		cString += '<ICMS>'
		cString += '<ICMSSN'  +cGrupo+'>'
		cString += '<orig>'   +aImposto[nX]:_Cpl:_orig:TEXT+'</orig>'
		cString += '<CSOSN>'    +aImposto[nX]:_Tributo:_CSOSN:TEXT+'</CSOSN>'		                       
		If aImposto[nX]:_Tributo:_CSOSN:TEXT$"900"
			cString += '<modBC>'+aImposto[nX]:_Tributo:_modBC:TEXT+'</modBC>'
			cString += '<vBC>'  +ConvType(Val(aImposto[nX]:_Tributo:_vBC:TEXT),15,2)+'</vBC>'
			cString += NfeTag('<pRedBC>'  ,"ConvType(Val(aImposto[nY]:_pRedBC:TEXT),5,2)")
			cString += '<pICMS>'+ConvType(Val(aImposto[nX]:_Tributo:_pICMS:TEXT),5,2)+ '</pICMS>'
			cString += '<vICMS>'+ConvType(Val(aImposto[nX]:_Tributo:_vICMS:TEXT),15,2)+'</vICMS>'
		EndIf      
		If aImposto[nX]:_Tributo:_CSOSN:TEXT$"201,202,203,900"
			cString += '<modBCST>'    +aImposto[nX]:_Tributo:_modBCST:TEXT+'</modBCST>'				
			cString += NfeTag('<pMVAST>'  ,"ConvType(Val(aImposto[nY]:_pMVAST:TEXT),5,2)")				
			cString += NfeTag('<pRedBCST>'  ,"ConvType(Val(aImposto[nY]:_pRedBCST:TEXT),5,2)")
			cString += '<vBCST>'  +ConvType(Val(aImposto[nX]:_Tributo:_vBCST:TEXT),15,2)+  '</vBCST>'                 
			cString += '<pICMSST>'+ConvType(Val(aImposto[nX]:_Tributo:_pICMSST:TEXT),5,2)+ '</pICMSST>'
			cString += '<vICMSST>'+ConvType(Val(aImposto[nX]:_Tributo:_vICMSST:TEXT),15,2)+'</vICMSST>'
		EndIF                                       		
		If aImposto[nX]:_Tributo:_CSOSN:TEXT$"500,900"
			cString += '<vBCSTRet>'    +ConvType(Val(aImposto[nX]:_Tributo:_vBCSTRet:TEXT),15,2)+  '</vBCSTRet>'                 				
			cString += '<vICMSSTRet>'  +ConvType(Val(aImposto[nX]:_Tributo:_vICMSSTRet:TEXT),15,2)+'</vICMSSTRet>'
		EndIF								
		If aImposto[nX]:_Tributo:_CSOSN:TEXT$"101,201,900"
			cString += '<pCredSN>'  +ConvType(Val(aImposto[nX]:_Tributo:_pCredSN:TEXT),5,2)+       '</pCredSN>'
			cString += '<vCredICMSSN>'+ConvType(Val(aImposto[nX]:_Tributo:_vCredICMSSN:TEXT),15,2)+'</vCredICMSSN>'
		EndIF
		cString += '</ICMSSN'+cGrupo+'>'
		cString += '</ICMS>'	
	EndIf
EndIf
nX := Ascan(aImposto,{|o| o:_Codigo:TEXT == "IPI"})
If nX > 0
	cString += '<IPI>'
	cString += NfeTag('<clEnq>'   ,"aImposto[nX]:_Cpl:_clEnq:TEXT")
	cString += NfeTag('<CNPJProd>',"aImposto[nX]:_Cpl:_CNPJProd:TEXT")
	cString += NfeTag('<cSelo>'   ,"aImposto[nX]:_Cpl:_cSelo:TEXT")
	cString += NfeTag('<qSelo>'   ,"aImposto[nX]:_Cpl:_qSelo:TEXT")
	If Type("aImposto[nX]:_Cpl:_cEnq:TEXT")=="U"
		cString += '<cEnq>999</cEnq>'
	Else
		cString += NfeTag('<cEnq>'    ,"aImposto[nX]:_Cpl:_cEnq:TEXT")
	EndIf
	If aImposto[nX]:_Tributo:_CST:TEXT$"00,49,50,99"
		cString += '<IPITrib>'
		cString += '<CST>'  +aImposto[nX]:_Tributo:_CST:TEXT+'</CST>'
		If cVersao >= "2.00"
			cString += '<vBC>'  +ConvType(Val(aImposto[nX]:_Tributo:_vBC:TEXT),15,2)+'</vBC>'                                                                          
			cString += NfeTag('<pIPI>' ,"ConvType(Val(aImposto[nX]:_Tributo:_Aliquota:TEXT),5,2)",.T.)
		EndIF	
		If (Type("aImposto[nX]:_Tributo:_vlTrib:TEXT")<>"U" .And. Val(aImposto[nX]:_Tributo:_vlTrib:TEXT)>0 .And.;
			(Type("aImposto[nX]:_Tributo:_modBC:TEXT")=="U" .Or. Empty(aImposto[nX]:_Tributo:_modBC:TEXT)) .Or.;
			(Type("aImposto[nX]:_Tributo:_modBC:TEXT")<>"U" .And. AllTrim(aImposto[nX]:_Tributo:_modBC:TEXT)$'12'))
			If cVersao < "1.10"
				cString += NfeTag('<qUnid>',"ConvType(Val(aImposto[nX]:_Tributo:_qTrib:TEXT),15,2)")
			Else
				cString += NfeTag('<qUnid>',"ConvType(Val(aImposto[nX]:_Tributo:_qTrib:TEXT),16,4)")
			EndIf
			cString += NfeTag('<vUnid>',"ConvType(Val(aImposto[nX]:_Tributo:_vlTrib:TEXT),15,4)")
		Else
			If cVersao <= "1.10"
				cString += '<vBC>'  +ConvType(Val(aImposto[nX]:_Tributo:_vBC:TEXT),15,2)+'</vBC>'
				cString += NfeTag('<pIPI>' ,"ConvType(Val(aImposto[nX]:_Tributo:_Aliquota:TEXT),5,2)",.T.)
			EndIf
		EndIf
		cString += NfeTag('<vIPI>' ,"ConvType(Val(aImposto[nX]:_Tributo:_valor:TEXT),15,2)",.T.)
		cString += '</IPITrib>'
		
		aImp[3][1] += Val(IIf(Type("aImposto[nX]:_Tributo:_vBC:TEXT")  =="U","0",aImposto[nX]:_Tributo:_vBC:TEXT))
		aImp[3][2] += Val(IIf(Type("aImposto[nX]:_Tributo:_valor:TEXT")=="U","0",aImposto[nX]:_Tributo:_valor:TEXT))
	Else
		cString += '<IPINT>'
		cString += '<CST>'+aImposto[nX]:_Tributo:_CST:TEXT+'</CST>'
		cString += '</IPINT>'
	EndIf
	cString += '</IPI>'
EndIf
nX := Ascan(aImposto,{|o| o:_Codigo:TEXT == "II"})
If nX > 0	
	cString += '<II>'
	cString += '<vBC>'      +ConvType(Val(aImposto[nX]:_Tributo:_vBC:TEXT),15,2)+'</vBC>'
	cString += '<vDespAdu>' +ConvType(Val(aImposto[nX]:_Cpl:_vDespAdu:TEXT),15,2)+'</vDespAdu>'
	cString += '<vII>'      +ConvType(Val(aImposto[nX]:_Tributo:_Valor:TEXT),15,2)+'</vII>'
	cString += '<vIOF>'     +ConvType(Val(aImposto[nX]:_Cpl:_vIOF:TEXT),15,2)+'</vIOF>'
	cString += '</II>'
	
	aImp[4][1] += Val(IIf(Type("aImposto[nX]:_Tributo:_vBC:TEXT")  =="U","0",aImposto[nX]:_Tributo:_vBC:TEXT))
	aImp[4][2] += Val(IIf(Type("aImposto[nX]:_Tributo:_valor:TEXT")=="U","0",aImposto[nX]:_Tributo:_valor:TEXT))
EndIf
nX := Ascan(aImposto,{|o| o:_Codigo:TEXT == "PIS"})
If nX > 0
	lPIS := .T.
	cString += '<PIS>'
	If aImposto[nX]:_Tributo:_CST:TEXT $ "01,02"
		cString += '<PISAliq>'
		cString += '<CST>'    +aImposto[nX]:_Tributo:_CST:TEXT+'</CST>'
		cString += '<vBC>'    +ConvType(Val(aImposto[nX]:_Tributo:_VBC:TEXT),15,2)+'</vBC>'
		cString += '<pPIS>'   +ConvType(Val(aImposto[nX]:_Tributo:_Aliquota:TEXT),5,2)+'</pPIS>'
		cString += '<vPIS>'   +ConvType(Val(aImposto[nX]:_Tributo:_Valor:TEXT),15,2)+'</vPIS>'
		cString += '</PISAliq>'

		aImp[5][1] += Val(IIf(Type("aImposto[nX]:_Tributo:_vBC:TEXT")  =="U","0",aImposto[nX]:_Tributo:_vBC:TEXT))
		aImp[5][2] += Val(IIf(Type("aImposto[nX]:_Tributo:_valor:TEXT")=="U","0",aImposto[nX]:_Tributo:_valor:TEXT))
		nValPis    := Val(IIf(Type("aImposto[nX]:_Tributo:_valor:TEXT")=="U","0",aImposto[nX]:_Tributo:_valor:TEXT))		
	EndIf
	If aImposto[nX]:_Tributo:_CST:TEXT $ "03"
		cString += '<PISQtde>'
		cString += '<CST>'      +aImposto[nX]:_Tributo:_CST:TEXT+'</CST>'
		If cVersao < "1.10"
			cString += '<qBCProd>'  +ConvType(Val(aImposto[nX]:_Tributo:_qTrib:TEXT),15,2)+'</qBCProd>'
		Else
			cString += '<qBCProd>'  +ConvType(Val(aImposto[nX]:_Tributo:_qTrib:TEXT),16,4)+'</qBCProd>'
		EndIf
		cString += '<vAliqProd>'+ConvType(Val(aImposto[nX]:_Tributo:_VlTrib:TEXT),15,4)+'</vAliqProd>'
		cString += '<vPIS>'     +ConvType(Val(aImposto[nX]:_Tributo:_Valor:TEXT),15,2)+'</vPIS>'
		cString += '</PISQtde>'

		aImp[5][1] += Val(IIf(Type("aImposto[nX]:_Tributo:_vBC:TEXT")  =="U","0",aImposto[nX]:_Tributo:_vBC:TEXT))
		aImp[5][2] += Val(IIf(Type("aImposto[nX]:_Tributo:_valor:TEXT")=="U","0",aImposto[nX]:_Tributo:_valor:TEXT))
		nValPis    := Val(IIf(Type("aImposto[nX]:_Tributo:_valor:TEXT")=="U","0",aImposto[nX]:_Tributo:_valor:TEXT))			
	EndIf
	If aImposto[nX]:_Tributo:_CST:TEXT $ "04,06,07,08,09"
		cString += '<PISNT>'
		cString += '<CST>'    +aImposto[nX]:_Tributo:_CST:TEXT+'</CST>'
		cString += '</PISNT>'
	EndIf
	If aImposto[nX]:_Tributo:_CST:TEXT $ "99"
		cString += '<PISOutr>'
		cString += '<CST>'    +aImposto[nX]:_Tributo:_CST:TEXT+'</CST>'
		If (Type("aImposto[nX]:_Tributo:_vlTrib:TEXT")<>"U" .And. Val(aImposto[nX]:_Tributo:_vlTrib:TEXT)>0 .And.;
			(Type("aImposto[nX]:_Tributo:_modBC:TEXT")=="U" .Or. Empty(aImposto[nX]:_Tributo:_modBC:TEXT)) .Or.;
			(Type("aImposto[nX]:_Tributo:_modBC:TEXT")<>"U" .And. AllTrim(aImposto[nX]:_Tributo:_modBC:TEXT)$'12'))				
			If cVersao < "1.10"
				cString += '<qBCProd>'  +ConvType(Val(aImposto[nX]:_Tributo:_qTrib:TEXT),15,2)+'</qBCProd>'
			Else
				cString += '<qBCProd>'  +ConvType(Val(aImposto[nX]:_Tributo:_qTrib:TEXT),16,4)+'</qBCProd>'
			EndIf
			cString += '<vAliqProd>'+ConvType(Val(aImposto[nX]:_Tributo:_vlTrib:TEXT),15,4)+'</vAliqProd>
		Else
			cString += '<vBC>'      +ConvType(Val(aImposto[nX]:_Tributo:_vBC:TEXT),15,2)+'</vBC>'
			cString += '<pPIS>'     +ConvType(Val(aImposto[nX]:_Tributo:_Aliquota:TEXT),5,2)+'</pPIS>'
		EndIf
		cString += '<vPIS>'   +ConvType(Val(aImposto[nX]:_Tributo:_Valor:TEXT),15,2)+'</vPIS>'
		cString += '</PISOutr>'
		
		aImp[5][1] += Val(IIf(Type("aImposto[nX]:_Tributo:_vBC:TEXT")  =="U","0",aImposto[nX]:_Tributo:_vBC:TEXT))
		aImp[5][2] += Val(IIf(Type("aImposto[nX]:_Tributo:_valor:TEXT")=="U","0",aImposto[nX]:_Tributo:_valor:TEXT))
		nValPis    := Val(IIf(Type("aImposto[nX]:_Tributo:_valor:TEXT")=="U","0",aImposto[nX]:_Tributo:_valor:TEXT))		
	EndIf
	cString += '</PIS>'			
EndIf
If !lPIS
	cString += '<PIS>'
	cString += '<PISNT>'
	cString += '<CST>08</CST>'
	cString += '</PISNT>'
	cString += '</PIS>'
EndIf
nX := Ascan(aImposto,{|o| o:_Codigo:TEXT == "PISST"})
If nX > 0	
	If Val(aImposto[nX]:_Tributo:_Valor:TEXT)<>0
		cString += '<PISST>'
		If (Type("aImposto[nX]:_Tributo:_vlTrib:TEXT")<>"U" .And. Val(aImposto[nX]:_Tributo:_vlTrib:TEXT)>0 .And.;
			(Type("aImposto[nX]:_Tributo:_modBC:TEXT")=="U" .Or. Empty(aImposto[nX]:_Tributo:_modBC:TEXT)) .Or.;
			(Type("aImposto[nX]:_Tributo:_modBC:TEXT")<>"U" .And. AllTrim(aImposto[nX]:_Tributo:_modBC:TEXT)$'12'))				
			If cVersao < "1.10"
				cString += '<qBCProd>'  +ConvType(Val(aImposto[nX]:_Tributo:_qTrib:TEXT),15,2)+'</qBCProd>'
			Else
				cString += '<qBCProd>'  +ConvType(Val(aImposto[nX]:_Tributo:_qTrib:TEXT),16,4)+'</qBCProd>'
			EndIf
			cString += '<vAliqProd>'+ConvType(Val(aImposto[nX]:_Tributo:_vlTrib:TEXT),15,4)+'</vAliqProd>
		Else
			cString += '<vBC>'    +ConvType(Val(aImposto[nX]:_Tributo:_vBC:TEXT),15,2)+'</vBC>'
			cString += '<pPIS>'   +ConvType(Val(aImposto[nX]:_Tributo:_Aliquota:TEXT),5,2)+'</pPIS>'
		EndIf
		cString += '<vPIS>'+ConvType(Val(aImposto[nX]:_Tributo:_Valor:TEXT),15,2)+'</vPIS>'
		cString += '</PISST>'
		aImp[6][1] += Val(IIf(Type("aImposto[nX]:_Tributo:_vBC:TEXT")  =="U","0",aImposto[nX]:_Tributo:_vBC:TEXT))
		aImp[6][2] += Val(IIf(Type("aImposto[nX]:_Tributo:_valor:TEXT")=="U","0",aImposto[nX]:_Tributo:_valor:TEXT))
	EndIf
EndIf
nX := Ascan(aImposto,{|o| o:_Codigo:TEXT == "COFINS"})
If nX > 0
	lCofins := .T.		
	cString += '<COFINS>'
	If aImposto[nX]:_Tributo:_CST:TEXT $ "01,02"
		cString += '<COFINSAliq>'
		cString += '<CST>'       +aImposto[nX]:_Tributo:_CST:TEXT+'</CST>'
		cString += '<vBC>'       +ConvType(Val(aImposto[nX]:_Tributo:_vBC:TEXT),15,2)+'</vBC>'
		cString += '<pCOFINS>'   +ConvType(Val(aImposto[nX]:_Tributo:_Aliquota:TEXT),5,2)+'</pCOFINS>'
		cString += '<vCOFINS>'   +ConvType(Val(aImposto[nX]:_Tributo:_Valor:TEXT),15,2)+'</vCOFINS>'
		cString += '</COFINSAliq>'
	
		aImp[7][1] += Val(IIf(Type("aImposto[nX]:_Tributo:_vBC:TEXT")  =="U","0",aImposto[nX]:_Tributo:_vBC:TEXT))
		aImp[7][2] += Val(IIf(Type("aImposto[nX]:_Tributo:_valor:TEXT")=="U","0",aImposto[nX]:_Tributo:_valor:TEXT))
		nValCOF    := Val(IIf(Type("aImposto[nX]:_Tributo:_valor:TEXT")=="U","0",aImposto[nX]:_Tributo:_valor:TEXT))
	EndIf
	If aImposto[nX]:_Tributo:_CST:TEXT $ "03"
		cString += '<COFINSQtde>'
		cString += '<CST>'      +aImposto[nX]:_Tributo:_CST:TEXT+'</CST>'
		If cVersao < "1.10"
			cString += '<qBCProd>'  +ConvType(Val(aImposto[nX]:_Tributo:_qTrib:TEXT),15,2)+'</qBCProd>'
		Else
			cString += '<qBCProd>'  +ConvType(Val(aImposto[nX]:_Tributo:_qTrib:TEXT),16,4)+'</qBCProd>'
		EndIf
		cString += '<vAliqProd>'+ConvType(Val(aImposto[nX]:_Tributo:_vlTrib:TEXT),15,4)+'</vAliqProd>'
		cString += '<vCOFINS>'  +ConvType(Val(aImposto[nX]:_Tributo:_Valor:TEXT),15,2)+'</vCOFINS>'
		cString += '</COFINSQtde>'

		aImp[7][1] += Val(IIf(Type("aImposto[nX]:_Tributo:_vBC:TEXT")  =="U","0",aImposto[nX]:_Tributo:_vBC:TEXT))
		aImp[7][2] += Val(IIf(Type("aImposto[nX]:_Tributo:_valor:TEXT")=="U","0",aImposto[nX]:_Tributo:_valor:TEXT))
		nValCOF    := Val(IIf(Type("aImposto[nX]:_Tributo:_valor:TEXT")=="U","0",aImposto[nX]:_Tributo:_valor:TEXT))
	EndIf
	If aImposto[nX]:_Tributo:_CST:TEXT $ "04,06,07,08,09"
		cString += '<COFINSNT>'
		cString += '<CST>'    +aImposto[nX]:_Tributo:_CST:TEXT+'</CST>'
		cString += '</COFINSNT>'
	EndIf
	If aImposto[nX]:_Tributo:_CST:TEXT $ "99"
		cString += '<COFINSOutr>'
		cString += '<CST>'    +aImposto[nX]:_Tributo:_CST:TEXT+'</CST>'		
		If (Type("aImposto[nX]:_Tributo:_vlTrib:TEXT")<>"U" .And. Val(aImposto[nX]:_Tributo:_vlTrib:TEXT)>0 .And.;
			(Type("aImposto[nX]:_Tributo:_modBC:TEXT")=="U" .Or. Empty(aImposto[nX]:_Tributo:_modBC:TEXT)) .Or.;
			(Type("aImposto[nX]:_Tributo:_modBC:TEXT")<>"U" .And. AllTrim(aImposto[nX]:_Tributo:_modBC:TEXT)$'12'))				
			If cVersao < "1.10"
				cString += '<qBCProd>'  +ConvType(Val(aImposto[nX]:_Tributo:_qTrib:TEXT),15,2)+'</qBCProd>'
			Else
				cString += '<qBCProd>'  +ConvType(Val(aImposto[nX]:_Tributo:_qTrib:TEXT),16,4)+'</qBCProd>'
			EndIf
			cString += '<vAliqProd>'+ConvType(Val(aImposto[nX]:_Tributo:_vlTrib:TEXT),15,4)+'</vAliqProd>
		Else
			cString += '<vBC>'      +ConvType(Val(aImposto[nX]:_Tributo:_vBC:TEXT),15,2)+'</vBC>'
			cString += '<pCOFINS>'  +ConvType(Val(aImposto[nX]:_Tributo:_Aliquota:TEXT),5,2)+'</pCOFINS>'				
		EndIf
		cString += '<vCOFINS>'   +ConvType(Val(aImposto[nX]:_Tributo:_Valor:TEXT),15,2)+'</vCOFINS>'
		cString += '</COFINSOutr>'

		aImp[7][1] += Val(IIf(Type("aImposto[nX]:_Tributo:_vBC:TEXT")  =="U","0",aImposto[nX]:_Tributo:_vBC:TEXT))
		aImp[7][2] += Val(IIf(Type("aImposto[nX]:_Tributo:_valor:TEXT")=="U","0",aImposto[nX]:_Tributo:_valor:TEXT))
		nValCOF    := Val(IIf(Type("aImposto[nX]:_Tributo:_valor:TEXT")=="U","0",aImposto[nX]:_Tributo:_valor:TEXT))
	EndIf
	cString += '</COFINS>'
EndIf
If !lCofins
	cString += '<COFINS>'
	cString += '<COFINSNT>'
	cString += '<CST>08</CST>'
	cString += '</COFINSNT>'
	cString += '</COFINS>'
EndIf
nX := Ascan(aImposto,{|o| o:_Codigo:TEXT == "COFINSST"})
If nX > 0
	If Val(aImposto[nX]:_Tributo:_Valor:TEXT)<>0
		cString += '<COFINSST>'
		If (Type("aImposto[nX]:_Tributo:_vlTrib:TEXT")<>"U" .And. Val(aImposto[nX]:_Tributo:_vlTrib:TEXT)>0 .And.;
			(Type("aImposto[nX]:_Tributo:_modBC:TEXT")=="U" .Or. Empty(aImposto[nX]:_Tributo:_modBC:TEXT)) .Or.;
			(Type("aImposto[nX]:_Tributo:_modBC:TEXT")<>"U" .And. AllTrim(aImposto[nX]:_Tributo:_modBC:TEXT)$'12'))				
			If cVersao < "1.10"
				cString += '<qBCProd>'+ConvType(Val(aImposto[nX]:_Tributo:_qTrib:TEXT),15,2)+'</qBCProd>'
			Else
				cString += '<qBCProd>'+ConvType(Val(aImposto[nX]:_Tributo:_qTrib:TEXT),16,4)+'</qBCProd>'
			EndIf
			cString += '<vAliqProd>'+ConvType(Val(aImposto[nX]:_Tributo:_vlTrib:TEXT),15,4)+'</vAliqProd>	'
		Else
			cString += '<vBC>'+ConvType(Val(aImposto[nX]:_Tributo:_vBC:TEXT),15,2)+'</vBC>'
			cString += '<pCOFINS>'+ConvType(Val(aImposto[nX]:_Tributo:_Aliquota:TEXT),5,2)+'</pCOFINS>'
		EndIf
		cString += '<vCOFINS>'+ConvType(Val(aImposto[nX]:_Tributo:_Valor:TEXT),15,2)+'</vCOFINS>'
		cString += '</COFINSST>'			
		aImp[8][1] += Val(IIf(Type("aImposto[nX]:_Tributo:_vBC:TEXT")  =="U","0" ,aImposto[nX]:_Tributo:_vBC:TEXT))
		aImp[8][2] += Val(IIf(Type("aImposto[nX]:_Tributo:_valor:TEXT")=="U","0",aImposto[nX]:_Tributo:_valor:TEXT))
	EndIf
EndIf
nX := Ascan(aImposto,{|o| o:_Codigo:TEXT == "ISS"})
If nX > 0
	cString += '<ISSQN>'
	cString += '<vBC>'      +ConvType(Val(aImposto[nX]:_Tributo:_vBC:TEXT),15,2)+'</vBC>'
	cString += '<vAliq>'    +ConvType(Val(aImposto[nX]:_Tributo:_Aliquota:TEXT),5,2)+'</vAliq>'
	cString += '<vISSQN>'   +ConvType(Val(aImposto[nX]:_Tributo:_Valor:TEXT),15,2)+'</vISSQN>'
	cString += '<cMunFG>'   +aImposto[nX]:_Cpl:_cMunFg:TEXT+'</cMunFG>'
	If cVersao >= "1.10"
		cString += '<cListServ>'+aImposto[nX]:_Cpl:_cListServ:TEXT+'</cListServ>'
	EndIf
	If cVersao >= "2.00"
		cString += '<cSitTrib>'+aImposto[nX]:_Cpl:_cSitTrib:TEXT+'</cSitTrib>'
	EndIf
	cString += '</ISSQN>'

	aImp[9][1] += Val(IIf(Type("aImposto[nX]:_Tributo:_vBC:TEXT")  =="U","0" ,aImposto[nX]:_Tributo:_vBC:TEXT))
	aImp[9][2] += Val(IIf(Type("aImposto[nX]:_Tributo:_valor:TEXT")=="U","0",aImposto[nX]:_Tributo:_valor:TEXT))
	aImp[9][3] += Val(oDet:_Prod:_vProd:TEXT)
	aImp[9][4] += nValPis
	aImp[9][5] += nValCof
EndIf
cString += '</imposto>'
If !lCdata
	cString += NfeTag('<infAdProd>',"oXml:_infAdProd:TEXT")
Else                                                                                         
	cString += '<infAdProd>'+"<![CDATA[<![CDATA["+oXml:_ANFAVEAPROD:TEXT+"]]]]><![CDATA[>]]>"
	cString += IIF(Type("oXml:_infAdProd:TEXT")=="U","",oXml:_infAdProd:TEXT)
	cString +='</infAdProd>'
EndIf
cString += '</det>'
Return(cString)

Static Function XmlNfeTotal(cVersao,oTotal,aImp,aTot)

Local nX       := 0
Local aLacre   := {}
Private aAux   := aImp
Private aTrib  := {}
Private oXml   := oTotal
Private cString:= ""

cString += '<total>'
cString += '<ICMSTot>'
cString += '<vBC>'    +ConvType(aImp[1][1],15,2)+'</vBC>'
cString += '<vICMS>'  +ConvType(aImp[1][2],15,2)+'</vICMS>'
cString += '<vBCST>'  +ConvType(aImp[2][1],15,2)+'</vBCST>'
cString += '<vST>'    +ConvType(aImp[2][2],15,2)+'</vST>'
cString += '<vProd>'  +ConvType(aTot[1],15,2)+'</vProd>'
cString += '<vFrete>' +ConvType(aTot[2],15,2)+'</vFrete>'
cString += '<vSeg>'   +ConvType(aTot[3],15,2)+'</vSeg>'
cString += '<vDesc>'  +ConvType(aTot[4],15,2)+'</vDesc>'
cString += '<vII>'    +ConvType(aImp[4][2],15,2)+'</vII>'
cString += '<vIPI>'   +ConvType(aImp[3][2],15,2)+'</vIPI>'
cString += '<vPIS>'   +ConvType(aImp[5][2],15,2)+'</vPIS>'
cString += '<vCOFINS>'+ConvType(aImp[7][2],15,2)+'</vCOFINS>'
cString += '<vOutro>' +ConvType(Val(oTotal:_Despesa:TEXT),15,2)+'</vOutro>'
cString += '<vNF>'    +ConvType(Val(oTotal:_vNF:TEXT),15,2)+'</vNF>'
cString += '</ICMSTot>'
If aImp[9][2]>0
	cString += '<ISSQNtot>'
	cString += NfeTag('<vServ>'  ,"ConvType(aAux[9][3],15,2)")	
	cString += NfeTag('<vBC>'    ,"ConvType(aAux[9][1],15,2)")
	cString += NfeTag('<vISS>'   ,"ConvType(aAux[9][2],15,2)")
	cString += NfeTag('<vPIS>'   ,"ConvType(aAux[9][4],15,2)")
	cString += NfeTag('<vCOFINS>',"ConvType(aAux[9][5],15,2)")
	cString += '</ISSQNtot>'
EndIf
If Type("oXml:_TributoRetido")<>"U"
	If Type("oXml:_TributoRetido")=="A"
		aTrib := oTotal:_TributoRetido
	Else
		aTrib := {oTotal:_TributoRetido}
	EndIf
	cString += '<retTrib>'	
	nX := Ascan(aTrib,{|o| o:_Codigo:TEXT == "PIS"})
	If nX > 0
		cString += '<vRetPIS>'+ConvType(Val(aTrib[nX]:_Valor:TEXT),15,2)+'</vRetPIS>'
	EndIf
	nX := Ascan(aTrib,{|o| o:_Codigo:TEXT == "COFINS"})
	If nX > 0
		cString += '<vRetCOFINS>'+ConvType(Val(aTrib[nX]:_Valor:TEXT),15,2)+'</vRetCOFINS>'
	EndIf
	nX := Ascan(aTrib,{|o| o:_Codigo:TEXT == "CSLL"})
	If nX > 0
		cString += '<vRetCSLL>'+ConvType(Val(aTrib[nX]:_Valor:TEXT),15,2)+'</vRetCSLL>'
	EndIf
	nX := Ascan(aTrib,{|o| o:_Codigo:TEXT == "IRRF"})
	If nX > 0
		cString += '<vBCIRRF>'+ConvType(Val(aTrib[nX]:_BC:TEXT),15,2)+'</vBCIRRF>'
		cString += '<vIRRF>'+ConvType(Val(aTrib[nX]:_Valor:TEXT),15,2)+'</vIRRF>'
	EndIf
	nX := Ascan(aTrib,{|o| o:_Codigo:TEXT == "INSS"})
	If nX > 0	
		cString += '<vBCRetPrev>'+ConvType(Val(aTrib[nX]:_BC:TEXT),15,2)+'</vBCRetPrev>'
		If type ("aTrib[nX]:_Valor:TEXT")<>"U"
			cString += '<vRetPrev>'+ConvType(Val(aTrib[nX]:_Valor:TEXT),15,2)+'</vRetPrev>'
		EndIf
	EndIf
	cString += '</retTrib>'
EndIf
cString += '</total>'
Return(cString)

Static Function XmlNfeTransp(cVersao,oTransp)
        
Local nZ        := 0
Local nY        := 0
Private aVol    := {}
Private nX      := 0
Private oXml    := oTransp
Private aReboque := {}
cString := ""

cString += '<transp>'
cString += '<modFrete>'+oXml:_ModFrete:TEXT+'</modFrete>'
If Type("oXml:_Transporta")<>"U"
	cString += '<transporta>'
	cString += NfeTag('<CNPJ>'  ,"oXml:_Transporta:_CNPJ:TEXT")
	cString += NfeTag('<CPF>'   ,"oXml:_Transporta:_CPF:TEXT")
	cString += NfeTag('<xNome>' ,"oXml:_Transporta:_Nome:TEXT")
	cString += NfeTag('<IE>'    ,"oXml:_Transporta:_IE:TEXT")
	cString += NfeTag('<xEnder>',"oXml:_Transporta:_Ender:TEXT")
	cString += NfeTag('<xMun>'  ,"oXml:_Transporta:_Mun:TEXT")
	cString += NfeTag('<UF>'    ,"oXml:_Transporta:_UF:TEXT")
	cString += '</transporta>'
EndIf
If Type("oXml:_RetTransp")<>"U" .And. Val(oXml:_RetTransp:_Tributo:_Valor:TEXT)>0
	cString += '<retTransp>'
	cString += '<vServ>'   +ConvType(Val(oXml:_RetTransp:_Cpl:_Valor:TEXT),15,2)+'</vServ>'
	cString += '<vBCRet>'  +ConvType(Val(oXml:_RetTransp:_Tributo:_vBC:TEXT),15,2)+'</vBCRet>'
	cString += '<pICMSRet>'+ConvType(Val(oXml:_RetTransp:_Tributo:_Aliquota:TEXT),15,2)+'</pICMSRet>'
	cString += '<vICMSRet>'+ConvType(Val(oXml:_RetTransp:_Tributo:_Valor:TEXT),15,2)+'</vICMSRet>'
	cString += '<CFOP>'    +oXml:_RetTransp:_Cpl:_CFOP:TEXT+'</CFOP>'
	cString += '<cMunFG>'  +oXml:_RetTransp:_Cpl:_cMunFG:TEXT+'</cMunFG>'
	cString += '</retTransp>'
EndIf
If Type("oXml:_Veictransp")<>"U"
	cString += '<veicTransp>'
	cString += '<placa>'+oXml:_Veictransp:_Placa:TEXT+'</placa>'
	cString += '<UF>'   +oXml:_Veictransp:_UF:TEXT+'</UF>'
	cString += NfeTag('<RNTC>',"oXml:_Veictransp:_RNTC:TEXT")
	cString += '</veicTransp>'
EndIf
If Type("oXml:_Reboque")<>"U"
	If Type("oXml:_Reboque")=="A"
		aReboque := oXml:_Reboque
    Else
        aReboque := {oXml:_Reboque}
    EndIf
    For nZ := 1 To Min(2,Len(aReboque))
		nX := nZ
        cString += '<reboque>'
        cString += '<placa>'+aReboque[nX]:_Placa:TEXT+'</placa>'
        cString += '<UF>'   +aReboque[nX]:_UF:TEXT+'</UF>'
        cString += NfeTag('<RNTC>',"aReboque[nX]:_RNTC:TEXT")
        If cVersao >= "2.00"
	       cString += NfeTag('<vagao>',"aReboque[nX]:_vagao:TEXT")
   	       cString += NfeTag('<balsa>',"aReboque[nX]:_balsa:TEXT")
	 	EndIf   
        cString += '</reboque>'
 	Next nZ
EndIf
cString += NfeTag('<RNTC>',"oXml:_vagao:TEXT")
cString += NfeTag('<RNTC>',"oXml:_balsa:TEXT")
If Type("oXml:_Vol")<>"U"
	If ValType(oXml:_Vol)=="A"
		aVol := oXml:_Vol
	Else
		aVol := {oXml:_Vol}
	EndIf
	For nZ := 1 To Len(aVol)		
		nX := nZ
		cString += '<vol>'
		cString += NfeTag('<qVol>'  ,"ConvType(Val(aVol[nX]:_qVol:TEXT),15,0)")
		cString += NfeTag('<esp>'   ,"aVol[nX]:_esp:TEXT")
		cString += NfeTag('<marca>' ,"aVol[nX]:_Marca:TEXT")
		cString += NfeTag('<nVol>'  ,"aVol[nX]:_nVol:TEXT")
		cString += NfeTag('<pesoL>' ,"ConvType(Val(aVol[nX]:_pesol:TEXT),15,3)")
		cString += NfeTag('<pesoB>' ,"ConvType(Val(aVol[nX]:_pesob:TEXT),15,3)")
		If Type("aVol[nX]:_Lacres")<>"U"
			If ValType(aVol[nX]:_Lacres) == "A"
				aLacres := aVol[nX]:_Lacres
			Else
				aLacres := {aVol[nX]:_Lacres}
			EndIf
			For nY := 1 To Len(aLacres)
				cString += '<lacres>'
				cString += '<nLacre>'+aLacres[nY]:_LACRE:TEXT+'</nLacre>'
				cString += '</lacres>'
			Next nY
		EndIf
		cString += '</vol>'
	Next nX
EndIf
cString += '</transp>'
Return(cString)

Static Function XmlNfeCob(cVersao,oDupl)

Local nZ        := 0
Private oXml    := oDupl
Private aDupl   := {} 
Private nX      := 0
cString := ""

If oDupl <> Nil
	If ValType(oDupl:_Dup)=="A"
		aDupl := oDupl:_Dup
	Else
		aDupl := {oDupl:_Dup}
	EndIf
	cString += '<cobr>'
	For nZ := 1 To Len(aDupl)
		nX := nZ
		cString += '<dup>'
		cString += '<nDup>' +aDupl[nX]:_Dup:TEXT+'</nDup>'
		cString += '<dVenc>'+aDupl[nX]:_dtVenc:TEXT+'</dVenc>'
		cString += '<vDup>' +ConvType(Val(aDupl[nX]:_vDup:TEXT),15,2)+'</vDup>'
		cString += '</dup>'
	Next nX	
	cString += '</cobr>'
EndIf
Return(cString)

Static Function XmlNfeInf(cVersao,oInf,lCdata)
Local nZ        := 0
Local cCpl		:= ""
Default lCdata  := .F.
Private nX      := 0
Private oXml    := oInf
cString := ""
If oInf <> Nil
	cString += '<infAdic>'
	If cVersao < "1.11"
		cString += NfeTag('<infAdFisco>',"ConvType(oXml:_FISCO:TEXT,256,0)")
	Else
		cString += NfeTag('<infAdFisco>',"ConvType(oXml:_FISCO:TEXT,2000,0)")
	EndIf
	If cVersao < "1.10"
		cString += NfeTag('<infCpl>',"oXml:_Cpl:TEXT")
	Else
		If 	Type("oXml:_ANFAVEACPL:TEXT")=="U" .And. Type("oXml:_Cpl")<>"U" .And. !Empty("oXml:_Cpl:TEXT") 
			cString += NfeTag('<infCpl>',"ConvType(oXml:_Cpl:TEXT,5000,0)")
		ElseIf Type("oXml:_ANFAVEACPL:TEXT")<>"U"
			cString += '<infCpl>'
			cString +="<![CDATA[<![CDATA["+oXml:_ANFAVEACPL:TEXT+"]]]]><![CDATA[>]]>"
			cString += IIF(Type("oXml:_Cpl:TEXT")=="U","",ConvType(oXml:_Cpl:TEXT,5000,0))
			cString +='</infCpl>'
		EndIf
	EndIf

	If Type("oXml:_obsCont")<>"U"
		If Type("oXml:_obsCont")=="A"
			aObsCont := oXml:_obsCont
	    Else
	        aObsCont := {oXml:_obsCont}
	    EndIf
	    For nZ := 1 To Len(aObsCont) // conforme manual da SEFAZ possibilita ter informacoes somente 10 TAG's obsCont
			nX := nZ 
			If nx <= 10
				cXcampo := Convtype(aObsCont[nX]:_xCampo:TEXT,20)
		        cString += '<obsCont xCampo="'+cXcampo+'">'
		        cString += '<xTexto>'+Convtype(aObsCont[nX]:_xTexto:TEXT,60)+'</xTexto>'
		        cString += '</obsCont>'
		  	Else
		  		Exit
		  	EndIf
	 	Next nZ
	EndIf
	cString += '</infAdic>'	
EndIf
Return(cString)

Static Function XmlNfeExp(cVersao,oExp)

Private oXml    := oExp
cString := ""
If oExp <> Nil
	cString += '<exporta>'
	cString += NfeTag('<UFEmbarq>',"oXml:_UFEmbarq:TEXT")
	cString += NfeTag('<xLocEmbarq>',"oXml:_locembarq:TEXT")
	cString += '</exporta>'	
EndIf
Return(cString)


Static Function XmlNfeInfCompra(cVersao,oCompra)

Private oXml    := oCompra
cString := ""
If oCompra <> Nil .And. cVersao >= "2.00"
	cString += '<compra>'
	cString += NfeTag('<xNEmp>',"oXml:_NEmp:TEXT")
	cString += NfeTag('<xPed>',"oXml:_Pedido:TEXT")
	cString += NfeTag('<xCont>',"oXml:_Contrato:TEXT")
	cString += '</compra>'
EndIf
Return(cString)

//Funcao para geracao das informacoes do Registro de Aquisicao de Cana previsto na Versao 2.00 da Nf-e.
Static Function XmlNfeCana(cVersao,oCana)
Local nX :=0
Local nZ :=0
Private aForDia := {}
Private aDeduc	:= {}
Private oXml    := oCana
cString := ""

If oCana <> Nil .And. cVersao >= "2.00"
	cString += '<cana>'
	cString += '<safra>'+oXml:_safra:TEXT+'</safra>'
	cString += '<ref>'+oXml:_ref:TEXT+'</ref>'
	If Type("oXml:_forDia")<>"U"
		If Type("oXml:_forDia")=="A"
			aForDia := oXml:_forDia
		Else
			aForDia := {oXml:_forDia}
		EndIf
		For nZ := 1 To Len(aForDia) // conforme manual da SEFAZ possibilita ter informacoes somente 31 TAG's forDia
			nX := nZ
			If nx <= 31    
				cString += '<forDia dia="'+aForDia[nX]:_dia:Text+'">'
				cString += '<qtde>'+ConvType(Val(aForDia[nX]:_qtde:Text),11,10)+'</qtde>'
				cString += '</forDia>'
			Else
				Exit
			EndIf
		Next nZ
	EndIf
	cString += '<qTotMes>'+ConvType(Val(oXml:_qTotMes:Text),11,10)+'</qTotMes>'
	cString += '<qTotAnt>'+ConvType(Val(oXml:_qTotAnt:Text),11,10)+'</qTotAnt>'
	cString += '<qTotGer>'+ConvType(Val(oXml:_qTotGer:Text),11,10)+'</qTotGer>' 
	If Type("oXml:_deduc")<>"U"
		If Type("oXml:_deduc")=="A"
			aDeduc := oXml:_deduc
		Else
			aDeduc := {oXml:_deduc}
		EndIf
		For nZ := 1 To Len(aDeduc) // conforme manual da SEFAZ possibilita ter informacoes somente 10 TAG's deduc
			If nz <= 10    
				cString += '<deduc>'
				cString += '<xDed>'+ConvType(aDeduc[nX]:_xDed:Text,60)+'</xDed>'           
				cString += '<vDed>'+ConvType(Val(aDeduc[nX]:_vDed:Text),15,2)+'</vDed>'
				cString += '</deduc>'
			Else
				Exit
			EndIf
		Next nZ
	EndIF	
	cString += '<vFor>'+ConvType(Val(oXml:_vFor:Text),15,2)+'</vFor>'
	cString += '<vTodDed>'+ConvType(Val(oXml:_vTodDed:Text),15,2)+'</vTodDed>'
	cString += '<vLiqFor>'+ConvType(Val(oXml:_vLiqFor:Text),15,2)+'</vLiqFor>'
	cString += '</cana>'
EndIf
Return(cString)                                      

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Funcao    ³GetUFCode ³ Rev.  ³Eduardo Riera          ³ Data ³11.05.2007³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡…o ³Funcao de recuperacao dos codigos de UF do IBGE             ³±±
±±³          ³                                                            ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Parametros³ExpC1: Codigo do Estado ou UF                               ³±±
±±³          ³ExpC2: lForceUf                                             ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Retorno   ³Nenhum                                                      ³±±
±±³          ³                                                            ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡„o ³Esta funcao tem como objetivo retornar o codigo do IBGE da  ³±±
±±³          ³UF                                                          ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Observacao³                                                            ³±±
±±³          ³                                                            ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Uso       ³ Totvs SPED Services Gateway                                ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/
Static Function GetUFCode(cUF,lForceUF)

Local nX         := 0
Local cRetorno   := ""
Local aUF        := {}
DEFAULT lForceUF := .F.

aadd(aUF,{"RO","11"})
aadd(aUF,{"AC","12"})
aadd(aUF,{"AM","13"})
aadd(aUF,{"RR","14"})
aadd(aUF,{"PA","15"})
aadd(aUF,{"AP","16"})
aadd(aUF,{"TO","17"})
aadd(aUF,{"MA","21"})
aadd(aUF,{"PI","22"})
aadd(aUF,{"CE","23"})
aadd(aUF,{"RN","24"})
aadd(aUF,{"PB","25"})
aadd(aUF,{"PE","26"})
aadd(aUF,{"AL","27"})
aadd(aUF,{"SE","28"})
aadd(aUF,{"BA","29"})
aadd(aUF,{"MG","31"})
aadd(aUF,{"ES","32"})
aadd(aUF,{"RJ","33"})
aadd(aUF,{"SP","35"})
aadd(aUF,{"PR","41"})
aadd(aUF,{"SC","42"})
aadd(aUF,{"RS","43"})
aadd(aUF,{"MS","50"})
aadd(aUF,{"MT","51"})
aadd(aUF,{"GO","52"})
aadd(aUF,{"DF","53"})

If !Empty(cUF)
	nX := aScan(aUF,{|x| x[1] == cUF})
	If nX == 0
		nX := aScan(aUF,{|x| x[2] == cUF})
		If nX <> 0
			cRetorno := aUF[nX][1]
		EndIf
	Else
		cRetorno := aUF[nX][IIF(!lForceUF,2,1)]
	EndIf
Else
	cRetorno := aUF
EndIf
Return(cRetorno)

Static Function ConvType(xValor,nTam,nDec)

Local cNovo := ""
DEFAULT nDec := 0
Do Case
	Case ValType(xValor)=="N"
		If xValor <> 0
			cNovo := AllTrim(Str(xValor,nTam+1,nDec))	
			If Len(cNovo)>nTam
				cNovo := AllTrim(Str(xValor,nTam+1,nDec-(Len(cNovo)-nDec)))
			EndIF				 			
		Else
			cNovo := "0"
		EndIf
	Case ValType(xValor)=="D"
		cNovo := FsDateConv(xValor,"YYYYMMDD")
		cNovo := SubStr(cNovo,1,4)+"-"+SubStr(cNovo,5,2)+"-"+SubStr(cNovo,7)
	Case ValType(xValor)=="C"
		If nTam==Nil
			xValor := AllTrim(xValor)
		EndIf
		DEFAULT nTam := 60
		cNovo := AllTrim(EnCodeUtf8(NoAcento(SubStr(xValor,1,nTam))))
EndCase
Return(cNovo)
 

Static Function NfeTag(cTag,cConteudo,lBranco)

Local cRetorno := ""
Local lBreak   := .F.
Local bErro    := ErrorBlock({|e| lBreak := .T. })
DEFAULT lBranco := .F.
Begin Sequence
	cConteudo := &(cConteudo)
	If lBreak
		BREAK
	EndIf	
Recover
	If lBranco
		cConteudo := ""
	Else
		cConteudo := Nil
	EndIf
End Sequence
ErrorBlock(bErro)
If cConteudo<>Nil .And. ((!Empty(AllTrim(cConteudo)) .And. (HasAlpha(AllTrim(cConteudo))) .Or. Val(AllTrim(cConteudo))<>0) .Or. lBranco)
	cRetorno := cTag+AllTrim(cConteudo)+SubStr(cTag,1,1)+"/"+SubStr(cTag,2)
EndIf
Return(cRetorno)    


Static Function HasAlpha(cTexto)
Local lRetorno := .F.
Local cAux     := ""

While !Empty(cTexto)
	cAux := SubStr(cTexto,1,1)
	If Asc(cAux) > 64 .And. Asc(cAux) < 123
		lRetorno := .T.
		cTexto := ""
	EndIf
		cTexto := SubStr(cTexto,2)
EndDo
Return(lRetorno)

Static Function GetTssVersao()
Return("2.01")
