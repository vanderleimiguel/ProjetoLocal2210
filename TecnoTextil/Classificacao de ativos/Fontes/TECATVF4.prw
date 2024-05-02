#Include "Protheus.ch"
#INCLUDE "TOTVS.CH"

/*/{Protheus.doc} TECATVF4
Funcao para atualizar codigo e calculo da SN1
@author Wagner Neves
@since 18/03/2024
@version 1.0
@type function
/*/
User Function TECATVF4()
	Local aArea     := GetArea()
    Local cAliasSN1 := GetNextAlias()
    Local cAliasGrp := GetNextAlias()
    Local cQuery1 	:= ""
    Local cQuery2 	:= ""
    Local cGrupo    := ""
    Local cCodBem   := ""
    Local cItem     := ""
    Local cCodNovo  := ""
    Local cDoc      := ""
    Local cSerie    := ""
    Local cFornece  := ""
    Local cLoja     := ""
    Local cProduto  := ""
    Local cTES      := ""
    Local nSD1Total := 0
    Local nSD1Desc  := 0
    Local nSD1VIcm  := 0
    Local nSD1VIpi  := 0
    Local nSD1Imp5  := 0
    Local nSD1Imp6  := 0
    Local nSD1IcmC  := 0
    Local nCalculo  := 0
    Local cAlqImp6  := ""
    Local cAlqImp5  := ""
    Local cCODBCC   := ""
    Local cCSTPIS   := ""
    Local cCSTCOF   := ""
    Local cGrpDesc  := ""
    Local cCContab  := ""
    Local nTotSN1   := 0
    Local nAtual    := 0

    //Query busca ativos com codigo de bem NFE
    cQuery1 := "SELECT N1_CBASE, N1_ITEM, N1_GRUPO, N1_PRODUTO, N1_NFISCAL, N1_NSERIE, N1_FORNEC, N1_LOJA, N1_STATUS "
    cQuery1 += "FROM "+RetSqlName("SN1") + " SN1 "
    cQuery1 += "WHERE N1_CBASE LIKE 'NFE%' "
    cQuery1 += "AND N1_STATUS='1' "
    cQuery1 += "AND SN1.D_E_L_E_T_=' ' "
    cQuery1 += "ORDER BY N1_GRUPO, N1_CBASE, N1_ITEM "
    cQuery1 := ChangeQuery(cQuery1)
	dbUseArea(.T.,"TOPCONN",TcGenQry(,,cQuery1),cAliasSN1,.F.,.T.)
	(cAliasSN1)->(DbGoTop())

    Count To nTotSN1
    ProcRegua(nTotSN1)
    (cAliasSN1)->(DbGoTop())

    While !(cAliasSN1)->(EoF())
        
        //Incrementa regua
        nAtual++
		IncProc("Atualizando registro " + cValToChar(nAtual) + " de " + cValToChar(nTotSN1) + "...")

        cGrupo      := (cAliasSN1)->N1_GRUPO
        cCodBem     := (cAliasSN1)->N1_CBASE
        cItem       := (cAliasSN1)->N1_ITEM
        cDoc        := (cAliasSN1)->N1_NFISCAL
        cSerie      := (cAliasSN1)->N1_NSERIE
        cFornece    := (cAliasSN1)->N1_FORNEC
        cLoja       := (cAliasSN1)->N1_LOJA
        cProduto    := (cAliasSN1)->N1_PRODUTO
        cCodNovo    := ""
        nCalculo    := 0
        cAlqImp6    := ""
        cAlqImp5    := ""
        cCSTPIS     := ""
        cCSTCOF     := ""
        cCODBCC     := ""
        cGrpDesc    := ""
        cCContab    := ""
        
        //Query que conta, quantidade de cadastros do grupo na SN1
        cQuery2 := "SELECT MAX(N1_CBASE) AS QTDREG "
        cQuery2 += "FROM "+RetSqlName("SN1") + " SN1 "
        cQuery2 += "WHERE N1_GRUPO='" + cGrupo + "' "
        cQuery2 += "AND N1_CBASE LIKE '" + cGrupo + "%" + "' "
        cQuery2 += "AND SN1.D_E_L_E_T_=' ' "
        cQuery2 := ChangeQuery(cQuery2)

        If Select((cAliasGrp)) > 0
		    (cAliasGrp)->(DbClosearea())
	    EndIf

        dbUseArea(.T.,"TOPCONN",TcGenQry(,,cQuery2),cAliasGrp,.F.,.T.)
        (cAliasGrp)->(DbGoTop())

        //Tratamento de codigo do bem
        If Empty((cAliasGrp)->QTDREG)
            cCodNovo := cGrupo + "000001"
        else
            cCodNovo := Soma1((cAliasGrp)->QTDREG)
        EndIf

        If !Empty(cCodNovo)
            //Verifica dados na SD1 para executar calculo
   			SD1->(DbSetOrder(1))
			If SD1->(DbSeek(FWXFilial('SD1') + cDoc + cSerie + cFornece + cLoja + cProduto))
                nSD1Total := SD1->D1_TOTAL
                nSD1Desc  := SD1->D1_VALDESC
                nSD1VIcm  := SD1->D1_VALICM
                nSD1VIpi  := SD1->D1_VALIPI
                nSD1Imp5  := SD1->D1_VALIMP5
                nSD1Imp6  := SD1->D1_VALIMP6
                nSD1IcmC  := SD1->D1_ICMSCOM
                cTES      := SD1->D1_TES
                cAlqImp6  := SD1->D1_ALQIMP6
                cAlqImp5  := SD1->D1_ALQIMP5

                //Busca dados na SF4 para executar calculo
                SF4->(DBSetOrder(1))
                IF SF4->(DBSeek(FWXFilial('SF4') + cTES))
                    cCredIcm    := SF4->F4_CREDICM
                    cCredIpi    := SF4->F4_CREDIPI
                    cCODBCC     := SF4->F4_CODBCC
                    cCSTPIS     := SF4->F4_CSTPIS
                    cCSTCOF     := SF4->F4_CSTCOF
                
                    nCalculo    := (nSD1Total-nSD1Desc)-(If(cCredIpi=="S",nSD1VIpi,0)+IIf(cCredIcm=="S",nSD1VIcm,0);
                                    +nSD1Imp5+nSD1Imp6)+nSD1IcmC
                
                EndIf
            EndIf

            //Atualiza codigo do bem e calculo da SN1
            SN1->(DbSetOrder(1))
            If SN1->(DBSeek(FWXFilial('SN1') + cCodBem + cItem))
                RecLock("SN1", .F.)	
                SN1->N1_CBASE   := cCodNovo
                SN1->N1_VLAQUIS := nCalculo
                SN1->N1_ALIQPIS := cAlqImp6
                SN1->N1_ALIQCOF := cAlqImp5
                SN1->N1_ORIGCRD := "0"
                SN1->N1_CSTPIS  := cCSTPIS
                SN1->N1_CSTCOFI := cCSTCOF
                SN1->N1_CODBCC  := cCODBCC
                SN1->(MsUnLock())
            EndIf

            //Atualiza codigo do bem da SN2
            SN2->(DbSetOrder(1))
            If SN2->(DBSeek(FWXFilial('SN2') + cCodBem + cItem))
                While cCodBem + cItem = SN2->(N2_CBASE+N2_ITEM)
                    RecLock("SN2", .F.)	
                    SN2->N2_CBASE   := cCodNovo
                    SN2->(MsUnLock())
                    SN2->(DbSkip())
                EndDo
            EndIf
            
            //Atualiza codigo do bem da SN3
            SN3->(DbSetOrder(1))
            If SN3->(DBSeek(FWXFilial('SN3') + cCodBem + cItem))
                While cCodBem + cItem = SN3->(N3_CBASE+N3_ITEM)

                    SNG->(DBSetOrder(1))
	                IF SNG->(DBSeek(FWXFilial('SNG') + cGrupo))
                        cGrpDesc    := SNG->NG_DESCRIC
                        cCContab    := SNG->NG_CCONTAB
                    EndIf

                    RecLock("SN3", .F.)	
                    SN3->N3_CBASE   := cCodNovo
                    SN3->N3_VORIG1  := nCalculo
                    SN3->N3_CCONTAB := cCContab
                    SN3->N3_HISTOR  := AllTrim(cGrpDesc)
                    SN3->(MsUnLock())
                    SN3->(DbSkip())
                EndDo
            EndIf

            //Atualiza codigo do bem da SN4
            SN4->(DbSetOrder(1))
            If SN4->(DBSeek(FWXFilial('SN4') + cCodBem + cItem))
                While cCodBem + cItem = SN4->(N4_CBASE+N4_ITEM)
                    RecLock("SN4", .F.)	
                    SN4->N4_CBASE   := cCodNovo
                    SN4->(MsUnLock())
                    SN4->(DbSkip())
                EndDo
            EndIf 
        EndIf  
    	
        (cAliasSN1)->(DbSkip())

	EndDo

    If nTotSN1 = 0
        MsgInfo("Nao foram encontrados registros para serem atualizados", "Atualizacao de Ativos")
    Else
        MsgInfo("Foram atualizados " + cValToChar(nTotSN1) + ", registros!", "Atualizacao de Ativos")
    EndIf

    RestArea(aArea)

Return
