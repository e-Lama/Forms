#include "parser.ch"

FUNCTION prepare_form(xCode, xJson)

    IF PCount() == 2 .AND. ValType(xCode) == 'A'
        RETURN Parser():prepare_form_from_record(xCode, xJson)
    ELSE
        xJson := hb_JsonDecode(dbVariables->json)

        IF ValType(xJson) != 'H'
            xJson := hb_Hash()
        ENDIF
    ENDIF

    IF PCount() > 0 .AND. ValType(xCode) == 'A'
        RETURN Parser():prepare_form_from_record(xCode, xJson)
    ENDIF

RETURN IF(Empty(field->code), .T., Parser():prepare_form_from_record(hb_ATokens(field->code, OBJECT_SEPARATOR), xJson))

FUNCTION validate(xCode, xJson)

    IF PCount() == 2 .AND. ValType(xCode) == 'A'
        RETURN Parser():check_correctness(xCode, xJson)
    ELSE
        xJson := hb_JsonDecode(dbVariables->json)

        IF ValType(xJson) != 'H'
            xJson := hb_Hash()
        ENDIF
    ENDIF

    IF PCount() > 0 .AND. ValType(xCode) == 'A'
        RETURN Parser():prepare_form_from_record(xCode, xJson)
    ENDIF

RETURN IF(Empty(field->code), .T., Parser():check_correctness(hb_ATokens(field->code, OBJECT_SEPARATOR), xJson))

FUNCTION important_form(cID)

    LOCAL acImportantForms := {'CREATE_FORM', 'SET_DISTINCT_NAME', 'WHERE_MOVE', 'SWAP', 'GET_VARIABLE'}

RETURN AScan(acImportantForms, AllTrim(cId)) != 0


