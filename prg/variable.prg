#include "hbclass.ch"
#include "inkey.ch"
#include "box.ch"

#include "functions.ch"
#include "variables.ch"

#define VARIABLES_PATH 'dbVariables.dbf'

CREATE CLASS Variable 

EXPORTED:

    METHOD new(cName, lVariableID, axValues, acPossibleTypes, abValidates, lAlwaysTrim, nType, nIndex) CONSTRUCTOR
    METHOD get_value() INLINE ::__axValues[::__nIndex]
    METHOD get_type() INLINE ValType(::__axValues[::__nIndex])
    METHOD get_name() INLINE ::__cName
    METHOD is_variable_id() INLINE ::__lVariableID

    METHOD set_value(xValue) INLINE ::__axValues[::__nIndex] := xValue

    METHOD edit()

    METHOD to_string(lVariable, cName)

    METHOD handle_variables()

    OPERATOR '++' INLINE ++::__axValues[::__nIndex], Self
    OPERATOR '--' INLINE --::__axValues[::__nIndex], Self
    OPERATOR '+=' ARG xArg INLINE ::__axValues[::__nIndex] += xArg, Self
    OPERATOR '-=' ARG xArg INLINE ::__axValues[::__nIndex] -= xArg, Self

HIDDEN:

    VAR __cName AS CHARACTER
    VAR __lVariableID AS LOGICAL INIT .F.
    VAR __axValues AS ARRAY INIT Array(0)
    VAR __acPossibleTypes AS ARRAY INIT Array(0)
    VAR __abValidates AS ARRAY INIT Array(0)
    VAR __lAlwaysTrim AS LOGICAL INIT .F.
    VAR __nType AS NUMERIC INIT NONE

    VAR __nIndex AS NUMERIC INIT 1

    METHOD __remove_spaces(cVariable)
    METHOD __get_type_array()

    METHOD __change_value(nIndex, lUpdated)
    METHOD __change_type(lUpdated)
    METHOD __changeable_type() INLINE Len(::__acPossibleTypes) > 1

    METHOD __create_variables_file()

ENDCLASS LOCKED

METHOD new(cName, lVariableID, axValues, acPossibleTypes, abValidates, lAlwaysTrim, nType, nIndex) CLASS Variable

    ::__cName := cName
    ::__axValues := axValues
    ::__acPossibleTypes := acPossibleTypes
    ::__abValidates := abValidates
    ::__lVariableID := lVariableID

    IF ValType(lAlwaysTrim) == 'L'
        ::__lAlwaysTrim := lAlwaysTrim
    ENDIF

    IF ValType(nIndex) == 'N'
        ::__nIndex := nIndex
    ENDIF

    IF ValType(nType) == 'N'
        ::__nType := nType
    ENDIF

RETURN Self

METHOD to_string(lVariable, cName) CLASS Variable

    LOCAL xValue := ::__axValues[::__nIndex]
    LOCAL cType := ValType(xValue)
    LOCAL cString := IF(lVariable, 'V', 'C') + cType

    IF lVariable
        IF ValType(cName) == 'C'
            RETURN cString + cName
        ELSE
            RETURN cString + ::__cName
        ENDIF
    ELSE
        DO CASE
            CASE cType == 'C'
                cString += xValue
            CASE cType == 'N'
                cString += LTrim(Str(xValue))
            CASE cType == 'L' 
                cString += IF(xValue, '.T.', '.F.')
            CASE cType == 'D'
                cString += DToS(xValue)
            CASE cType == 'A'
                cString += hb_JsonEncode(hb_Hash('', xValue))
            OTHERWISE
                throw(ARGUMENT_TYPE_EXCEPTION)
        ENDCASE
    ENDIF

RETURN cString

METHOD __change_value(nIndex, lUpdated) CLASS Variable

    MEMVAR GETLIST

    LOCAL nOldWindow := WSelect()
    LOCAL nOldCursor := Set(_SET_CURSOR)
    LOCAL cOldHeader := Window():header(Config():get_config('MemoEditHeader'))
    LOCAL nTop := Window():get_top() + 1
    LOCAL nLeft := Window():get_left() + 1
    LOCAL nBottom := Window():get_bottom() - 1
    LOCAL nRight := Window():get_right() - 1
    LOCAL lContinue := .T.
    LOCAL xValue := ::__axValues[nIndex]
    LOCAL cType := ValType(xValue)
    LOCAL hVariables := hb_Hash()
    LOCAL cOldScreen
    LOCAL cOldFooter 

    cOldFooter := Window():footer(Config():get_config(IF(cType == 'C', 'ChangeValueColorBox', 'MemoEditFooter')))

    WSelect(0)
    SAVE SCREEN TO cOldScreen
    Window():refresh_header_footer()

    IF YesNo(Config():get_config('ChangeVariableValue'))

        IF cType == 'A'
            DO WHILE lContinue

                @ nTop, nLeft, nBottom, nRight BOX B_SINGLE
                @ nTop, Int(nRight + nLeft - Len(Config():get_config('Code'))) / 2 SAY Config():get_config('Code')
                xValue := hb_JsonDecode(MemoEdit(hb_jsonEncode(xValue, .T.), nTop + 1, nLeft + 1, nBottom - 1, nRight - 1))

                SET CURSOR (cast(nOldCursor, 'L'))

                IF Eval(::abValidates[nIndex], xValue)
                    lContinue := .F.
                ELSE
                    Inform(Config():get_config('IncorrectValueVariable'))
                    xValue := ::__axValues[nIndex]
                ENDIF
            ENDDO

        ELSE
            IF cType == 'C'
                xValue := xValue + Space(VARIABLE_CHARACTER_LENGTH - Len(xValue))

                SET KEY K_F2 TO select_color()
                SET KEY K_F3 TO select_box()

                IF ::__nType == IS_BOX
                   xValue := hb_Translate(xValue, 'EN', hb_cdpSelect())
                ENDIF
            ENDIF

            hVariables['variable'] := xValue

            SWITCH cType
                CASE 'C'
                    hVariables['picture'] := Replicate('X', Len(xValue))
                    EXIT
                CASE 'N'
                    hVariables['picture'] := Replicate('9', Len(Str(0)))
                    EXIT
                CASE 'L'
                    hVariables['picture'] := 'Y'
                    EXIT
                CASE 'D'
                    hVariables['picture'] := '@D'
                    EXIT
            ENDSWITCH

            GETLIST := {}

            IF Parser():prepare_form_from_database(Config():get_config('Language'), 'GET_VARIABLE', hVariables)
                READ
                hVariables := Parser():get_answers()
                WClose()
                WSelect(0)

                IF cType == 'N'
                    xValue := Val(hVariables['variable'])
                ELSEIF cType == 'D'
                    //...
                ELSE
                    xValue := cast(hVariables['variable'], cType)
                ENDIF
            ELSE
                throw(Config():get_config('CriticalError'))
            ENDIF
        ENDIF

        IF cType == 'A'
            lUpdated := !array_equals(xValue, ::__axValues[nIndex])
        ELSEIF xValue != ::__axValues[nIndex]
            lUpdated := .T.
        ENDIF

        IF cType == 'C'
            IF ::__lAlwaysTrim
                xValue := AllTrim(xValue)
            ELSE
               xValue := ::__remove_spaces(xValue)
            ENDIF
        ENDIF

        IF ::__nType == IS_BOX
            xValue := hb_Translate(xValue, hb_cdpSelect(), 'EN')
        ENDIF

    ENDIF

    Window():header(cOldHeader)
    Window():footer(cOldFooter)
    RESTORE SCREEN FROM cOldScreen
    Window():refresh_header_footer()
    WSelect(nOldWindow)

RETURN xValue

METHOD __get_type_array()

    LOCAL acArray := Array(0)
    LOCAL cType
    
    FOR EACH cType IN ::__acPossibleTypes
        SWITCH cType
            CASE 'N'
                AAdd(acArray, 'NUMERIC')
                EXIT
            CASE 'D'
                AAdd(acArray, 'DATE')
                EXIT
            CASE 'C'
                AAdd(acArray, 'CHARACTER')
                EXIT
            CASE 'L'
                AAdd(acArray, 'LOGICAL')
                EXIT
        ENDSWITCH
    NEXT

RETURN acArray

METHOD __change_type(lUpdated)

    LOCAL acMenuItems := ::__get_type_array()
    LOCAL nChoose := ::__nIndex

    IF YesNo(Config():get_config('ChangeVariableDataType'))

        nChoose := display_menu_center_autosize(Window():center_row(), Window():center_col(), acMenuItems, .T.;
                                               , 'menu_search_allow_exit_move', 1, Config():get_config('DefaultMenuColor');
                                               , Config():get_config('DefaultBox');
                                               )
        IF nChoose != 0
            lUpdated := .T.
        ENDIF
    ENDIF

RETURN nChoose

METHOD edit() CLASS Variable

    LOCAL nOldWindow := WSelect()
    LOCAL lUpdated := .F.
    LOCAL lSave := .F.
    LOCAL xNewValue
    LOCAL nNewIndex

    WSelect(0)

    IF ::__changeable_type() 
        nNewIndex := ::__change_type(@lUpdated)
    ELSE
        nNewIndex := ::__nIndex
    ENDIF

    IF nNewIndex != 0
        xNewValue := ::__change_value(nNewIndex, @lUpdated)

        IF lUpdated .AND. YesNo(Config():get_config('SaveVariableEditing'))
            ::__axValues[nNewIndex] := xNewValue
            ::__nIndex := nNewIndex
            lSave := .T.
        ENDIF
    ENDIF

    WSelect(nOldWindow)

RETURN lSave

METHOD __remove_spaces(cVariable) CLASS Variable

    LOCAL nChoose := Dialog(Config():get_config('DialogRemoveSpaces'), {Config():get_config('DefaultNo'), Config():get_config('FromLeft'), Config():get_config('FromRight'), Config():get_config('FromBoth')})

    DO CASE 
        CASE nChoose == 2
            cVariable := LTrim(cVariable)
        CASE nChoose == 3
            cVariable := RTrim(cVariable)
        CASE nChoose == 4
            cVariable := AllTrim(cVariable)
    ENDCASE

RETURN cVariable

METHOD handle_variables() CLASS Variable

    LOCAL cNoDataBaseFileDialog := Config():get_config('NoVariableFileDialog')
    LOCAL cNoDataBaseFileInform := Config():get_config('NoVariableFileInform')
    LOCAL lSuccess

    IF File(Config():get_config('dbfPath') + VARIABLES_PATH)
        USE (Config():get_config('dbfPath') + VARIABLES_PATH) VIA 'DBFNTX' ALIAS dbVariables NEW EXCLUSIVE 
        lSuccess := (Alias() == 'DBVARIABLES')
    ELSE
        IF YesNo(cNoDataBaseFileDialog)
            lSuccess := ::__create_variables_file()
            IF !lSuccess
                Inform(cNoDataBaseFileInform)
            ENDIF
        ELSE
            Inform(cNoDataBaseFileInform)
            lSuccess := .F.
        ENDIF
    ENDIF

    IF lSuccess
        INDEX ON field->language + field->id TO (Config():get_config('ntxPath') + 'dbVariablesInd1')
    ENDIF

RETURN lSuccess

METHOD __create_variables_file() CLASS Variable

    LOCAL axStruct := { ;
                      {'ID', 'C', 50, 0};
                      , {'LANGUAGE', 'C', 30, 0};
                      , {'JSON', 'M', 10, 0};
                      }

    dbCreate(Config():get_config('dbfPath') + VARIABLES_PATH, axStruct, 'DBFNTX', .T., 'dbVariables')

RETURN Alias() == 'DBVARIABLES'
