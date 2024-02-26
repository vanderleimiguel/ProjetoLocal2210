#Include "Protheus.ch"
#INCLUDE "TOTVS.CH"

/*/{Protheus.doc} TRWRREC1
Função Relatorio de Reclassificacao
@author Wagner Neves
@since 05/01/2024
@version 1.0
@type function
@obs Posições do ParamIXB:
    001 - aProd       -  Produto
    002 - cMensCli    -  Mensagem da nota
    003 - cMensFis    -  Mensagem padrao
    004 - aDest       -  Destinatario
    005 - aNota       -  Numero da nota
    006 - aInfoItem   -  Informações do Item
    007 - aDupl       -  Duplicata
    008 - aTransp     -  Transporte
    009 - aEntrega    -  Entrega
    010 - aRetirada   -  Retirada
    011 - aVeiculo    -  Veiculo
    012 - aReboque    -  Placa Reboque
    013 - aNfVincRur  -  Nota Produtor Rural Referenciada
    014 - aEspVol     -  Especie Volume
    015 - aNfVinc     -  NF Vinculada
    016 - aDetPag     -  
/*/
User Function PE01NFESEFAZ()
    Local aArea     := GetArea()
    Local aDados    := ParamIXB
    Local aProd     := aDados[1]
    Local cMensCli  := aDados[2]
    Local cMensFis  := aDados[3]
    Local aDest     := aDados[4]
    Local aNota     := aDados[5]
    Local aInfoItem := aDados[6]
    Local aDupl     := aDados[7]
    Local aTransp   := aDados[8]
    Local aEntrega  := aDados[9]
    Local aRetirada := aDados[10]
    Local aVeiculo  := aDados[11]
    Local aReboque  := aDados[12]
    Local aNfVincRur:= aDados[13]
    Local aEspVol   := aDados[14]
    Local aNfVinc   := aDados[15]
    Local aDetPag   := aDados[16]
    Local aObsCotAux:= aDados[17]
    Local cMenNota  := ""
    Local cXMenNot  := ""
    Local cImprime  := ""
   
    SC5->(DbSetOrder(1))
    SC5->(DbGoTop())
    If dbseek(xFilial("SC5") + SD2->D2_PEDIDO)
        cMenNota    := SC5->C5_MENNOTA
        cXMenNot    := SC5->C5_XMENNOT

        If Empty(cMenNota)
            cImprime := cXMenNot
        ElseIf !Empty(cMenNota) .AND. Empty(cXMenNot)
            cImprime    := cMenNota
        ElseIf !Empty(cMenNota) .AND. !Empty(cXMenNot)
            cImprime    := cMenNota + " - " + cXMenNot
        EndIf


       
    EndIf

    RestArea(aArea)
Return
