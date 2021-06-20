MODULE CommonModule 
    TYPE Word
        CHARACTER(LEN=20) :: value
    END TYPE Word
END MODULE

MODULE WordList
    USE CommonModule

    TYPE WordListItem
        TYPE(Word) :: word
        TYPE(WordListItem), POINTER :: next => NULL()
    END TYPE WordListItem

    CONTAINS 

    SUBROUTINE PrintWordList(head)
        TYPE(WordListItem), POINTER :: head
        TYPE(WordListItem), POINTER :: ptr
        INTEGER :: i
    
        IF(.NOT. ASSOCIATED(head)) THEN
            PRINT *, "NO WORDS FOUND"
            RETURN
        END IF

        ptr => head%next
    
        i = 1
        DO WHILE ( ASSOCIATED(ptr) )
          PRINT *, i, ") ", ptr%word
          ptr => ptr%next
          i = i + 1
        END DO
        PRINT *
    END SUBROUTINE PrintWordList

    SUBROUTINE InsertWordList(head, new_word)
        TYPE(WordListItem), POINTER :: head, ptr, temp
        TYPE(Word) :: new_word

        IF(.NOT. ASSOCIATED(head)) THEN
            ALLOCATE(head)
            head%word = new_word
            RETURN
        END IF
        
        ptr => head
        DO WHILE (ASSOCIATED(ptr%next))
            ptr => ptr%next
        END DO
        
        ALLOCATE(temp)
        temp%word = new_word

        ptr%next => temp
    END SUBROUTINE InsertWordList
END MODULE WordList

MODULE DictionaryModule
    USE CommonModule
    USE WordList

    INTEGER, PARAMETER :: DIGITS_COUNT = 8, START_DIGIT = 2, END_DIGIT = 9
    
    TYPE DictNode
        TYPE(DictNode), POINTER, DIMENSION(:) :: next => NULL()
        TYPE(WordListItem), POINTER :: word_list_root => NULL()
    END TYPE DictNode

    CONTAINS

    SUBROUTINE ShowWordsStartingWith(rootNode, digits_str)
        TYPE(DictNode), POINTER, INTENT(IN) :: rootNode
        CHARACTER(LEN=*), INTENT(IN) :: digits_str
        TYPE(DictNode), POINTER :: nodeIterator
        INTEGER :: digit, digits_count, i

        IF (.NOT. ASSOCIATED(rootNode)) THEN
            PRINT *, "NO WORDS FOUND"
            RETURN
        END IF

        nodeIterator = rootNode
        digits_count = LEN_TRIM(digits_str)
        DO i=1, digits_count
            READ (digits_str(i:i), *) digit
            nodeIterator = nodeIterator%next(digit)
            
            IF (.NOT. ASSOCIATED(nodeIterator)) THEN
                PRINT *, "NO WORDS FOUND"
                RETURN
            END IF
        END DO

        CALL PrintWordList(nodeIterator%word_list_root)
    END SUBROUTINE ShowWordsStartingWith

    SUBROUTINE InsertWordToDict(rootNode, word_str)
        TYPE(DictNode), POINTER :: rootNode
        CHARACTER(LEN=*) :: word_str
        TYPE(Word) :: new_word
        TYPE(DictNode), POINTER :: iteratorNode
        INTEGER, ALLOCATABLE, DIMENSION(:) :: out_digits
        INTEGER :: i, current_digit, out_digits_size

        out_digits = WordToDigits(word_str)
        out_digits_size = SIZE(out_digits)
                
        iteratorNode => rootNode
        DO i=1, out_digits_size    
            current_digit = out_digits(i)
            ! PRINT *, current_digit ! DEBUG

            IF(.NOT. ASSOCIATED(iteratorNode%next)) THEN
                ALLOCATE(iteratorNode%next(START_DIGIT:END_DIGIT))
            END IF

            iteratorNode => iteratorNode%next(current_digit)
        END DO

        IF(.NOT. ASSOCIATED(iteratorNode%word_list_root)) THEN
            ALLOCATE(iteratorNode%word_list_root)
        END IF

        new_word = Word(word_str)
        CALL InsertWordList(iteratorNode%word_list_root, new_word)
        
    END SUBROUTINE InsertWordToDict

    FUNCTION WordToDigits(word) RESULT(out_digits)
        CHARACTER(LEN=*), INTENT(IN) :: word
        INTEGER, ALLOCATABLE, DIMENSION(:) :: out_digits
        INTEGER :: i, digit_count
        
        digit_count = len(word)

        ! PRINT *, "word = ", trim(word) ! DEBUG
        ! PRINT *, "len = ", digit_count ! DEBUG

        ALLOCATE(out_digits(1:digit_count))
        DO i=1, digit_count
            out_digits(i) = CharToDigit(word(i:i))
        END DO
        
        RETURN
    END FUNCTION WordToDigits

    FUNCTION CharToDigit (char) RESULT (digit)
        CHARACTER, INTENT(IN) :: char
        INTEGER :: digit

        SELECT CASE (char)
            CASE ('a':'c', 'A':'C')
                digit = 2
            CASE ('d':'f', 'D':'F')
                digit = 3
            CASE ('g':'i', 'G':'I')
                digit = 4
            CASE ('j':'l', 'J':'L')
                digit = 5
            CASE ('m':'o', 'M':'O')
                digit = 6
            CASE ('p':'s', 'P':'S')
                digit = 7
            CASE ('t':'v', 'T':'V')
                digit = 8
            CASE ('w':'z', 'W':'Z')
                digit = 9
            CASE DEFAULT
                digit = -1
        END SELECT

        RETURN
    END FUNCTION CharToDigit
END MODULE DictionaryModule

PROGRAM p1
    USE CommonModule
    USE DictionaryModule
    USE WordList

    IMPLICIT NONE
    
    INTEGER, PARAMETER :: DICT_SIZE = 10000 
    INTEGER :: i, eof
    CHARACTER(LEN=20) :: current_word_str, prompt_str
    TYPE(DictNode), POINTER :: dict_root

    ALLOCATE(dict_root)

    OPEN (UNIT = 1, FILE = "dict.txt", STATUS = "OLD", ACTION = "READ", POSITION="REWIND")
    DO i=1, DICT_SIZE
        READ (UNIT=1, FMT=*, IOSTAT=eof) current_word_str
        
        CALL InsertWordToDict(dict_root, trim(current_word_str))

        IF (eof < 0) THEN
            EXIT
        END IF
    END DO


    DO while(.true.)
        READ(*,'(A)') prompt_str
        
        PRINT *, "Prompts: "
        CALL ShowWordsStartingWith(dict_root, prompt_str)
    END DO

    STOP
END PROGRAM p1
