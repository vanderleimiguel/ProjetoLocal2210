#Include "Protheus.ch"
#INCLUDE "TOTVS.CH"

/*/{Protheus.doc} TECATVF1
Funcao Gatilho para Atualizar Codigo do Bem
@author Wagner Neves
@since 18/03/2024
@version 1.0
@type function
/*/
User Function TECATVF1()
	Local aArea     := GetArea()
    Local cAliasSN1 := GetNextAlias()
    Local cGrupo    := M->N1_GRUPO
    Local nQtdReg   := 0
    Local cCodBem   := ""
    Local cQuery 	:= ""

    //Query que conta, quantidade de cadastros do grupo na SN1
	cQuery := "SELECT COUNT(*) AS QTDREG "
	cQuery += "FROM "+RetSqlName("SN1") + " SN1 "
	cQuery += "WHERE N1_GRUPO='" + cGrupo + "' "
	cQuery += "AND SN1.D_E_L_E_T_=' ' "
    cQuery := ChangeQuery(cQuery)
	dbUseArea(.T.,"TOPCONN",TcGenQry(,,cQuery),cAliasSN1,.F.,.T.)
	(cAliasSN1)->(DbGoTop())

    //Quantidade de registros encontrados
    nQtdReg := (cAliasSN1)->QTDREG
    //Tratamento de codigo do bem
    cCodBem := AllTrim(cGrupo) + PadL(cValToChar(nQtdReg + 1),6,"0")

    RestArea(aArea)
Return cCodBem
