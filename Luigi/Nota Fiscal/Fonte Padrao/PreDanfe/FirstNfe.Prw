#INCLUDE "PROTHEUS.CH"
#INCLUDE "COLORS.CH"
#INCLUDE "TBICONN.CH"
#INCLUDE "rwmake.ch"
#INCLUDE "TBICONN.CH"
#INCLUDE "dialog.ch"
#include "PRCONST.CH"

#DEFINE _OPC_cGETFILE ( GETF_RETDIRECTORY + GETF_LOCALHARD + GETF_NETWORKDRIVE)

User Function FISTRFNFE()

//aadd(aRotina,{'Gera XML Sem Envio','U_XMLTXT(MV_PAR01)' , 0 , 3,0,NIL})
aadd(aRotina,{'Gera Danfe Sem Envio','U_PREDANFE()' , 0 , 3,0,NIL})

Return Nil

User Function XMLTXT(_PAR01)
Local _cArqTxt:=""
Local _aArq:=GetArea()
Private aNotas    	:= {}

dbSelectArea("SF3")
dbSetOrder(4)
If SubStr(_PAR01,1,1) == "1"
	dbSeek(xFilial("SF2")+SF2->F2_CLIENTE+SF2->F2_LOJA+SF2->F2_DOC+SF2->F2_SERIE)
	_cTpNf:="1"
ELSE
	dbSeek(xFilial("SF1")+SF1->F1_FORNECE+SF1->F1_LOJA+SF1->F1_DOC+SF1->F1_SERIE)
	_cTpNf:="0"
endif
if Found()
	aadd(aNotas,{})
	nX := Len(aNotas)
	
	aadd(aNotas[nX],{_cTpNf,"",SF3->F3_SERIE,SF3->F3_NFISCAL,SF3->F3_CLIEFOR,SF3->F3_LOJA,{}})
	aadd(aNotas[nX],"3.10")
	aadd(aNotas[nX],"")
	aadd(aNotas[nX],{"",""})
	
	aXml := ExecBlock("XmlNfeSef",.F.,.F.,aNotas[nX])
	
	_cArqTxt:=aXml[2]
	
	if !empty(_cArqTxt)
		cPath     := cGetFile( "Selecione o Diretorio | " , OemToAnsi( "Diretório Temporário" ) , NIL , "" , .F. , _OPC_cGETFILE )
		Pg			:= ""
		cFim 		:= chr(13)+chr(10)
		cArqCli    	:= cPath+"NFE_"+alltrim(SF3->F3_NFISCAL)+"-"+alltrim(SF3->F3_SERIE)+".xml"
		nHdlCli		:= fCreate(cArqCli)
		cEOL 	    := "CHR(13)+CHR(10)"
		
		If Empty(cEOL)
			cEOL := CHR(13)+CHR(10)
		Else
			cEOL := Trim(cEOL)
			cEOL := &cEOL
		Endif
		
		If nHdlCli == -1
			MsgAlert("O arquivo de nome "+AllTrim(cArqCli)+" nao pode ser criado! Verifique os parametros.","Atencao!")
			fClose(nHdlCli)
			Return
		Endif
		
		Pg 	:=_cArqTxt + cFim ; fWrite(nHdlCli,Pg,Len(Pg))
		fClose(nHdlCli)
	Endif
	
Endif
RestArea(_aArq)
Return(_cArqTxt)
