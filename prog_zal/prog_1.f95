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
        type(WordListItem), pointer :: head
        type(WordListItem), pointer :: ptr
    
        if(.not. associated(head)) then
            print *, "NO WORDS FOUND"
            return
        end if

        ptr => head%next
    
        DO WHILE ( associated(ptr) )
          print *, ptr%word
          ptr => ptr%next
        END DO
        PRINT *
    END SUBROUTINE PrintWordList

    SUBROUTINE InsertWordList(head, new_word)
        type(WordListItem), pointer :: head, ptr, temp
        type(Word) :: new_word

        if(.not. associated(head)) then
            allocate(head)
            head%word = new_word
            return
        end if
        
        ptr => head
        DO WHILE ( associated(ptr%next) )
            ptr => ptr%next
        END DO
        
        allocate(temp)
        temp%word = new_word

        ptr%next => temp
    END SUBROUTINE InsertWordList
END MODULE WordList

MODULE DictionaryModule
    USE CommonModule
    USE WordList
    
    TYPE DictNode
        TYPE(DictNode), POINTER, DIMENSION(:) :: next => NULL()
        TYPE(WordListItem), POINTER :: word_list_root => NULL()
    END TYPE DictNode
END MODULE DictionaryModule

PROGRAM p1
    USE CommonModule
    USE DictionaryModule
    USE WordList

    IMPLICIT NONE
    
    INTEGER, PARAMETER :: DICT_SIZE = 10000 
    INTEGER, PARAMETER :: MAX_WORD_LEN = 20
    INTEGER, PARAMETER :: DIGITS_COUNT = 8, START_DIGIT = 2, END_DIGIT = 9

    INTEGER :: i, eof
    CHARACTER(LEN=20) :: current_val2
        
    TYPE(Word), DIMENSION(:), POINTER :: words_arr
    TYPE(Word), Dimension(DICT_SIZE) :: WordDict

    TYPE(WordListItem), POINTER :: list_head
    TYPE(WordListItem), POINTER :: temp_item

    TYPE(DictNode), POINTER :: dict_root
    allocate(dict_root)
    call InsertWordToDict(dict_root, "aeae")
    call InsertWordToDict(dict_root, "bebe")
    call PrintWordList(dict_root%next(2)%next(3)%next(2)%next(3)%word_list_root)


    allocate(list_head)

    ! call InsertWordList(list_head, Word("XD2"))
    ! call InsertWordList(list_head, Word("XD3"))
    ! call InsertWordList(list_head, Word("XD3"))
    ! call InsertWordList(list_head, Word("XD3"))
    ! call InsertWordList(list_head, Word("XD3"))
    ! call InsertWordList(list_head, Word("XD3"))
    ! call InsertWordList(list_head, Word("XD3"))
    ! call PrintWordList(list_head)


    ! Load dictionary

    OPEN (UNIT = 1, FILE = "dict.txt", STATUS = "OLD", ACTION = "READ", POSITION="REWIND")
    DO i=1, DICT_SIZE
        READ (UNIT=1, FMT=*, IOSTAT=eof) current_val2
        current_val2 = trim(current_val2)
    
        ! PRINT *, WordToDigits(current_val2)
        ! READ(*,*)
        
        ! WordDict(i) = String
        ! WordDict(i) = String(current_val)
        IF (eof < 0) THEN
            EXIT
        END IF
    END DO

    ! InsertWordToDict()

    CONTAINS

    SUBROUTINE InsertWordToDict(rootNode, word_str)
        IMPLICIT NONE
        TYPE(DictNode), POINTER :: rootNode
        CHARACTER(LEN=*) :: word_str
        TYPE(Word) :: new_word
        TYPE(DictNode), POINTER :: iteratorNode
        INTEGER, ALLOCATABLE, DIMENSION(:) :: out_digits
        INTEGER :: i, current_digit, out_digits_size

        out_digits = WordToDigits(word_str)
        out_digits_size = size(out_digits)
                
        iteratorNode => rootNode
        
        do i=1, out_digits_size    
            current_digit = out_digits(i)
            ! print *, current_digit ! DEBUG

            if(.not. associated(iteratorNode%next)) then
                allocate(iteratorNode%next(START_DIGIT:END_DIGIT))
            end if

            iteratorNode => iteratorNode%next(current_digit)
        end do

        if(.not. associated(iteratorNode%word_list_root)) then
            allocate(iteratorNode%word_list_root)
        end if

        new_word = Word(word_str)
        call InsertWordList(iteratorNode%word_list_root, new_word)
        
    END SUBROUTINE InsertWordToDict

    FUNCTION WordToDigits(word) RESULT(out_digits)
        CHARACTER(LEN=*), INTENT(IN) :: word
        INTEGER, ALLOCATABLE, DIMENSION(:) :: out_digits
        INTEGER :: i, digit_count
        
        digit_count = len(word)

        ! print *, "word = ", trim(word) ! DEBUG
        ! print *, "len = ", digit_count ! DEBUG

        allocate(out_digits(1:digit_count))
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

    ! STOP
END PROGRAM p1
