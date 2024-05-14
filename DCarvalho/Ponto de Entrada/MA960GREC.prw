#include "protheus.ch" 
 
 
//-------------------------------------------------------------------
/*/{Protheus.doc} MA960GREC
Ponto de Entrada para preenchimento dos campos F6_TIPOGNU, F6_DOCORIG, F6_DETRECE e F6_CODPROD de acordo com o código de receita e UF.
 
@author Vanderlei Miguel
@since 14/05/2024
/*/
//-------------------------------------------------------------------
User Function MA960GREC()
 
    Local aParam   := {0, '', '', 0, ''} //Parâmetros de retorno default
    //aParam Retorna os campos F6_TIPOGNU, F6_DOCORIG, F6_DETRECE, F6_CODPROD e F6_CODAREA de acordo com o código de receita e sigla da UF da guia atual.
    Local cReceita := PARAMIXB[1]    //Código de Receita da guia atual
    Local cUF      := PARAMIXB[2]    //Sigla da UF da guia atual
 
    If Alltrim(cReceita) $ '100099'
        Do Case
		    Case cUF $ 'AC'
                aParam := {10, '1', '',20, ''} 
		    Case cUF $ 'AL'
                aParam := {10, '1', '',20, ''} 
		    Case cUF $ 'AM'
                aParam := {22, '2', '',90, ''} 
		    Case cUF $ 'AP'
                aParam := {10, '1', '', 0, ''} 
		    Case cUF $ 'BA'
                aParam := {10, '1', '',20, ''} 
		    Case cUF $ 'CE'
                aParam := {10, '1', '',20, ''} 
		    Case cUF $ 'DF'
                aParam := {10, '1', '',20, ''} 
		    Case cUF $ 'GO'
                aParam := {10, '1', '',20, ''} 
		    Case cUF $ 'MA'
                aParam := {10, '1', '',90, ''} 
		    Case cUF $ 'MG'
                aParam := {10, '1', '', 0, ''} 
		    Case cUF $ 'MS'
                aParam := {10, '1', '',20, ''} 
		    Case cUF $ 'MT'
                aParam := {22, '2', '', 0, ''} 
		    Case cUF $ 'PA'
                aParam := {10, '1', '', 0, ''} 
		    Case cUF $ 'PB'
                aParam := { 0,  '', '', 0, ''} 
		    Case cUF $ 'PE'
                aParam := {22, '2', '',20, ''} 
		    Case cUF $ 'PI'
                aParam := {10, '1', '',20, ''} 
		    Case cUF $ 'PR'
                aParam := {10, '1', '', 0, ''} 
		    Case cUF $ 'RJ'
                aParam := {24, '2', '',90, ''} 
		    Case cUF $ 'RN'
                aParam := { 0,  '', '', 0, ''} 
		    Case cUF $ 'RO'
                aParam := {10, '1', '', 0, ''} 
		    Case cUF $ 'RR'
                aParam := {10, '1', '',20, ''} 
		    Case cUF $ 'RS'
                aParam := {22, '2', '', 0, ''} 
		    Case cUF $ 'SC'
                aParam := {24, '2', '', 0, ''} 
		    Case cUF $ 'SE'
                aParam := {10, '1', '', 0, ''} 
            Case cUF $ 'TO'
                aParam := {10, '1', '',20, ''} 
        EndCase         
    ElseIf Alltrim(cReceita) $ '100102'
        Do Case
		    Case cUF $ 'AC'
                aParam := {10, '1', '', 0, ''} 
		    Case cUF $ 'AL'
                aParam := {10, '1', '', 0, ''} 
		    Case cUF $ 'AM'
                aParam := {22, '2', '',90, ''} 
		    Case cUF $ 'AP'
                aParam := {10, '1', '', 0, ''} 
		    Case cUF $ 'BA'
                aParam := { 0,  '', '', 0, ''} 
		    Case cUF $ 'CE'
                aParam := {10, '1', '', 0, ''} 
		    Case cUF $ 'DF'
                aParam := {10, '1', '', 0, ''} 
		    Case cUF $ 'GO'
                aParam := {10, '1', '', 0, ''} 
		    Case cUF $ 'MA'
                aParam := {10, '1', '',90, ''} 
		    Case cUF $ 'MG'
                aParam := {10, '1', '', 0, ''} 
		    Case cUF $ 'MS'
                aParam := { 0,  '', '', 0, ''} 
		    Case cUF $ 'MT'
                aParam := {22, '2', '', 0, ''} 
		    Case cUF $ 'PA'
                aParam := {10, '1', '', 0, ''} 
		    Case cUF $ 'PB'
                aParam := { 0,  '', '', 0, ''} 
		    Case cUF $ 'PE'
                aParam := {24, '2', '', 0, ''} 
		    Case cUF $ 'PI'
                aParam := {10, '1', '', 0, ''} 
		    Case cUF $ 'PR'
                aParam := {10, '1', '', 0, ''} 
		    Case cUF $ 'RJ'
                aParam := {24, '2', '',90, ''} 
		    Case cUF $ 'RN'
                aParam := { 0,  '', '', 0, ''} 
		    Case cUF $ 'RO'
                aParam := {10, '1', '', 0, ''} 
		    Case cUF $ 'RR'
                aParam := {10, '1', '', 0, ''} 
		    Case cUF $ 'RS'
                aParam := {22, '2', '', 0, ''} 
		    Case cUF $ 'SC'
                aParam := {24, '2', '', 0, ''} 
		    Case cUF $ 'SE'
                aParam := {10, '1', '', 0, ''} 
            Case cUF $ 'TO'
                aParam := {10, '1', '', 0, ''} 
        EndCase          
    EndIf
    
Return aParam
