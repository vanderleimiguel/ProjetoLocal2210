//RTASK0018479-DEP-FECHADO-JUNDIAI - Renato Bandeira - 08/01/2024
#include "totvs.ch"

/*
====================================================================================
Programa............: PE01NFESEFAZ()
Autor...............: Flávio Dentello
Data................: 13/04/2017
Descricao / Objetivo: Ponto de entrada para inclusão de mensagens na DANFE
Doc. Origem.........: GAP - FIS29
Solicitante.........: Cliente
Uso.................: Marfrig
Obs.................:
=====================================================================================
*/

user function PE01NFESEFAZ()
	local aArea			:= getArea()
	local aAreaSC5		:= SC5->(getArea())
	local aAreaSC6		:= SC6->(getArea())
	local aAreaSF7		:= SF7->(getArea())
	local aAreaSF4		:= SF4->(getArea())
	local aAreaSA2		:= SA2->(getArea())
	local aAreaSA1		:= SA1->(getArea())
	local aAreaSM4		:= SM4->(getArea())
	Local aAreaDAK		:= DAK->(getArea())
	Local aAreaDA3		:= DA3->(getArea())
	Local aAreaSFT		:= SFT->(getArea())
	local aProd     	:= PARAMIXB[1]
	local cMensCli  	:= PARAMIXB[2] //+ CRLF
	local cMensFis  	:= PARAMIXB[3]
	local aDest     	:= PARAMIXB[4]
	local aNota     	:= PARAMIXB[5]
	local aInfoItem 	:= PARAMIXB[6]
	local aDupl     	:= PARAMIXB[7]
	local aTransp   	:= PARAMIXB[8]
	local aEntrega  	:= PARAMIXB[9]
	local aRetirada 	:= PARAMIXB[10]
	local aVeiculo  	:= PARAMIXB[11]
	local aReboque  	:= PARAMIXB[12]
	local aNfVincRur	:= PARAMIXB[13]
	Local aEspVol     	:= PARAMIXB[14]
	Local aNfVinc		:= PARAMIXB[15]
	Local aAdetPag		:= PARAMIXB[16]
	Local aObsCotAux    := PARAMIXB[17]
	local aRetorno		:= {}
	local aAreaCD2		:= CD2->(getArea())
	local aAreaSD2		:= SD2->(getArea())

	SC6->(DBGoTop())
	SC6->(dbSetOrder(4))
	SC6->( DbSeek( xFilial("SC6") + SF2->F2_DOC + SF2->F2_SERIE) )

	SC5->(DBGoTop())
	SC5->(dbSetOrder(1))
	SC5->( MsSeek( xFilial("SC5") + SC6->C6_NUM ) )

	If !Empty(SC5->C5_XMENNOT)
		cMensCli += " - " + ALLTRIM(SC5->C5_XMENNOT)
	EndIf

	cMensCli := STRTRAN(cMensCli,'"','')

	aadd( aRetorno, aProd		)
	aadd( aRetorno, cMensCli	)
	aadd( aRetorno, cMensFis	)
	aadd( aRetorno, aDest		)
	aadd( aRetorno, aNota		)
	aadd( aRetorno, aInfoItem	)
	aadd( aRetorno, aDupl		)
	aadd( aRetorno, aTransp		)
	aadd( aRetorno, aEntrega	)
	aadd( aRetorno, aRetirada	)
	aadd( aRetorno, aVeiculo	)
	aadd( aRetorno, aReboque	)
	aadd( aRetorno, aNfVincRur	)
	aadd( aRetorno, aEspVol 	)
	aadd( aRetorno, aNfVinc 	)
	aadd( aRetorno, aAdetPag)
	aadd( aRetorno, aObsCotAux  )

	restArea(aAreaSC6)
	restArea(aAreaSC5)
	restArea(aAreaSF7)
	restArea(aAreaSF4)
	restArea(aAreaSA2)
	restArea(aAreaSA1)
	restArea(aAreaCD2)
	restArea(aAreaSM4)
	restArea(aAreaSD2)
	restArea(aAreaDAK)
	restArea(aAreaDA3)
	restArea(aAreaSFT)
	restArea(aArea)

return aRetorno

