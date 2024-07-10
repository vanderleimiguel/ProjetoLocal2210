#Include "Protheus.ch"
#Include "rwmake.ch"

/*/{Protheus.doc} Fa473Cta
Ponto de Entrada para ajuste de Bancos
@author Wagner Neves / Vanderlei Miguel
@since 27/06/2024
@version 1.0
@type function
/*/
User Function Fa473Cta()
    Local cBanco    := SEE->EE_CODIGO
    Local cAgencia  := SEE->EE_AGENCIA
    Local cConta    := SEE->EE_CONTA
    Local aRet      := {}

    aRet := {cBanco, cAgencia, cConta}

 Return aRet
