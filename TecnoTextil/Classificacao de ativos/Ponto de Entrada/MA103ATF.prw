#Include 'Protheus.ch'

/*/{Protheus.doc} MA103ATF
MA103ATF - Manipulação do aCols e aItens enviados para Integração com Ativo Fixo
@author Wagner Neves
@since 27/03/2024
@version 1.0
@type function
/*/
User Function MA103ATF()
    Local aCab      := ParamIXB[1]
    Local aItens    := ParamIXB[2]
    Local _aCab     := {}

    //Chama rotina para atualizacao de dados SN1
    If ExistBlock("TECATVF2")
	    _aCab   := U_TECATVF2(aCab)
	EndIf

Return({_aCab,aItens})
