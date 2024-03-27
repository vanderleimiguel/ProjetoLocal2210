#Include 'Protheus.ch'

/*/{Protheus.doc} MA103ATF
MA103ATF - Manipulação do aCols e aItens enviados para Integração com Ativo Fixo
@author Wagner Neves
@since 27/03/2024
@version 1.0
@type function
/*/
User Function MA103ATF()
    Local aArea     := GetArea()
    Local aCab      := ParamIXB[1]
    Local aItens    := ParamIXB[2]
    Local cAliasSN1 := GetNextAlias()
    Local cCContab  := SD1->D1_CONTA
    Local cAlqImp5  := SD1->D1_ALQIMP5
    Local cAlqImp6  := SD1->D1_ALQIMP6
    Local cTES      := SD1->D1_TES
    Local cGrupo    := ""
    Local nQtdReg   := 0
    Local cCodBem   := ""
    Local cQuery 	:= ""
    Local nPosGrupo := 0
    Local nPosBase  := 0
    Local nCSTPIS   := 0
    Local nCSTCOF   := 0
    Local nCODBCC   := 0
    Local nALIQPIS  := 0
    Local nALIQCOF  := 0
    Local nORIGCRD  := 0

    //Alicota PIS
    nALIQPIS   := aScan(aCab, {|x| AllTrim(Upper(x[1])) == "N1_ALIQPIS"})
    If nALIQPIS = 0
        aAdd(aCab,{"N1_ALIQPIS" , cAlqImp6 })
    else
        aCab[nALIQPIS][2]   := cAlqImp6
    EndIf   

    //Alicota COFINS
    nALIQCOF   := aScan(aCab, {|x| AllTrim(Upper(x[1])) == "N1_ALIQCOF"})
    If nALIQCOF = 0
        aAdd(aCab,{"N1_ALIQCOF" , cAlqImp5 })
    else
        aCab[nALIQCOF][2]   := cAlqImp5
    EndIf   

    //Origem Credito
    nORIGCRD   := aScan(aCab, {|x| AllTrim(Upper(x[1])) == "N1_ORIGCRD"})
    If nORIGCRD = 0
        aAdd(aCab,{"N1_ORIGCRD" , "0" })
    else
        aCab[nORIGCRD][2]   := "0"
    EndIf 

    //Busca grupo atraves da conta contabil
    SNG->(DBSetOrder(3))
	IF SNG->(DBSeek(FWXFilial('SNG') + cCContab))
        cGrupo  := SNG->NG_GRUPO

        nPosGrupo   := aScan(aCab, {|x| AllTrim(Upper(x[1])) == "N1_GRUPO"})
        If nPosGrupo = 0
            aAdd(aCab,{"N1_GRUPO" , cGrupo })
        else
            aCab[nPosGrupo][2]   := cGrupo
        EndIf 

        SNG->(DBSetOrder(1))

        //Query que conta, quantidade de cadastros do grupo na SN1
        cQuery := "SELECT COUNT(*) AS QTDREG "
        cQuery += "FROM "+RetSqlName("SN1") + " SN1 "
        cQuery += "WHERE N1_GRUPO='" + cGrupo + "' "
        cQuery += "AND SN1.D_E_L_E_T_=' ' "
        cQuery := ChangeQuery(cQuery)
        dbUseArea(.T.,"TOPCONN",TcGenQry(,,cQuery),cAliasSN1,.F.,.T.)
        (cAliasSN1)->(DbGoTop())
    
        //Tratamento de codigo do bem
        nQtdReg := (cAliasSN1)->QTDREG
        cCodBem := AllTrim(cGrupo) + PadL(cValToChar(nQtdReg + 1),6,"0")

        //Grava codigo do Bem
        nPosBase    := aScan(aCab, {|x| AllTrim(Upper(x[1])) == "N1_CBASE"})
        If nPosBase = 0
            aAdd(aCab,{"N1_CBASE" , cCodBem })
        else
            aCab[nPosBase][2]   := cCodBem
        EndIf 
    EndIf

    //Busca tributos atraves da TES
    SF4->(DBSetOrder(1))
    IF SF4->(DBSeek(FWXFilial('SF4') + cTES))
        cCSTPIS := SF4->F4_CSTPIS
        cCSTCOF := SF4->F4_CSTCOF
        cCODBCC := SF4->F4_CODBCC

        //Cod.Sit.Trib. PIS
        nCSTPIS   := aScan(aCab, {|x| AllTrim(Upper(x[1])) == "N1_CSTPIS"})
        If nCSTPIS = 0
            aAdd(aCab,{"N1_CSTPIS" , cCSTPIS })
        else
            aCab[nCSTPIS][2]   := cCSTPIS
        EndIf

        //Cod.Sit.Trib. Cofins
        nCSTCOF   := aScan(aCab, {|x| AllTrim(Upper(x[1])) == "N1_CSTCOFI"})
        If nCSTCOF = 0
            aAdd(aCab,{"N1_CSTCOFI" , cCSTCOF })
        else
            aCab[nCSTCOF][2]   := cCSTCOF
        EndIf

        //Codigo BC do Credito
        nCODBCC   := aScan(aCab, {|x| AllTrim(Upper(x[1])) == "N1_CODBCC"})
        If nCODBCC = 0
            aAdd(aCab,{"N1_CODBCC" , cCODBCC })
        else
            aCab[nCODBCC][2]   := cCODBCC
        EndIf        
    EndIf
   
    RestArea(aArea)
Return({aCab,aItens})
