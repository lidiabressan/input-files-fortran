!------------------------------------------------------------------------------
!
! MODULE: StrNumConversion
!
!> @author
!> Lidia Bressan
!
!  2014 06 09
!
! DESCRIPTION:
!>
!>  Module for fast numeric to string conversion.
!>
!
! REVISION HISTORY:
! 2014 06 09 - Initial Version from separated files
!------------------------------------------------------------------------------


MODULE StrNumConversion
 IMPLICIT NONE




  INTERFACE NUM
   MODULE PROCEDURE NUM_r
  END INTERFACE NUM

  INTERFACE STR
   MODULE PROCEDURE STR_i
  END INTERFACE STR



contains




  FUNCTION STR_i(inte)
   !---------------------------------------------------------------------------
   ! DESCRIPTION:
   !>   FUNCTION STR (inte)
   !>
   !>   Return a string of the integer inte.
   !
   !> @param[in] inte
   !> @return string
   !---------------------------------------------------------------------------
   CHARACTER(len=10)::STR_i
   INTEGER,INTENT(IN)::inte
    WRITE(str_i,'(i10)') inte
    str_i=ADJUSTL(str_i)
  END FUNCTION STR_i




  FUNCTION num2c(inte)
   !---------------------------------------------------------------------------
   ! DESCRIPTION:
   !>   FUNCTION num2c (inte)
   !>
   !>   Return a string of 2 digit of the integer inte.
   !
   !> @param[in] inte
   !> @return string
   !---------------------------------------------------------------------------
   INTEGER::inte
   CHARACTER(len=2)::num2c

    IF(inte<10)THEN
        num2c='0'//trim(str(inte))
    ELSEIF(inte>=100)THEN
        num2c='ER'
    ELSE
        num2c=TRIM(str(inte))
    END IF
  ENDFUNCTION num2c




  FUNCTION num3c(inte)
   !---------------------------------------------------------------------------
   ! DESCRIPTION:
   !>   FUNCTION num3c (inte)
   !>
   !>   Return a string of 3 digit of the integer inte.
   !
   !> @param[in] inte
   !> @return STR
   !---------------------------------------------------------------------------
   INTEGER::inte
   CHARACTER(len=3)::num3c

    IF(inte<10)THEN
        num3c='00'//trim(str(inte))
    ELSEIF( inte>=10 .and. inte<100)THEN
        num3c='0'//trim(str(inte))
    ELSEIF(inte>=1000)THEN
        num3c='ERR'
    ELSE
        num3c=TRIM(str(inte))
    END IF
  ENDFUNCTION num3c




  FUNCTION NUM_r(stringa)
   !---------------------------------------------------------------------------
   ! DESCRIPTION:
   !>   FUNCTION NUM_r (stringa)
   !>
   !>   Find a number in a string and convert it to float.
   !>
   !>   It works only if there are at least 2 digit in the exponential part.
   !>
   !
   !> @param[in] strin
   !> @return num_r
   !---------------------------------------------------------------------------
   !funziona solo con almeno 2 cifre all'esponente
   REAL(KIND=8),PARAMETER::FMiss_default=-9999
   CHARACTER(LEN=*),INTENT(IN)::stringa
   REAL(KIND=8)::NUM_r
   integer::ii=0, kk=0, jj=0, np=0, ne=0
   INTEGER,DIMENSION(3)::ind1=0, ind2=0

    !! TROVA IL NUMERO NELLA STRINGA: ind1,ind2
    kk=0
    cerca: DO ii=1,LEN(stringa)
        IF(IACHAR(stringa(ii:ii))-47>=1 .and. IACHAR(stringa(ii:ii))-47<=10)THEN
        !!! se il carattere è una cifra
            kk=1
            ind1(kk)=ii  ! indice iniziale
            ind2(kk)=ii  ! indice finale
            EXIT CERCA
        END IF
    END DO CERCA
    IF(kk==0)THEN
        num_r=FMiss_default
        RETURN
    ELSE
        np=0  ! numero punti (per decimali)
        ne=0  ! numero di E o e per scrivere l'esponente (notazione scientifica)
        ind: DO jj=ind1(kk)+1,LEN(stringa)
            ! ! ! se il carattere è una cifra
            casi: IF(IACHAR(stringa(jj:jj))-47>=1 .and. IACHAR(stringa(jj:jj))-47<=10)THEN
                ind2(kk)=jj
            ! ! ! se np==0 e il carattere è un punto '.'(46) e esiste il carattere dopo
            ELSEIF(np==0 .AND. IACHAR(stringa(jj:jj))==46 .and. jj+1<=LEN(stringa))THEN casi
            ! ! ! se il carattere dopo è una cifra
                IF(IACHAR(stringa(jj+1:jj+1))-47>=1 .and. IACHAR(stringa(jj+1:jj+1))-47<=10 )THEN
                    np=1
                    ind2(kk)=jj
                ELSE
                    EXIT ind
                END IF
            ! ! ! se ne==0 e il carattere è 'E'(69) oppure 'e'(101) e esistono due caratteri dopo
            ELSEIF(ne==0 .and. (IACHAR(stringa(jj:jj))==69 .or. IACHAR(stringa(jj:jj))==101) .and. jj+2<=LEN(stringa))THEN casi
            ! ! ! se il carattere successivo è '+'(43) oppure '-'(45) e quello dopo ancora è una cifra Oppure
            ! ! ! se il carattere successivo è una cifra
                IF(((IACHAR(stringa(jj+1:jj+1))==43 .or. IACHAR(stringa(jj+1:jj+1))==45) .and.  &! + o -
                  & (IACHAR(stringa(jj+2:jj+2))-47>=1 .and. IACHAR(stringa(jj+2:jj+2))-47<=10)) .OR. &
                  & (IACHAR(stringa(jj+1:jj+1))-47>=1 .and. IACHAR(stringa(jj+1:jj+1))-47<=10))THEN
                    ne=1
                    ind2(kk)=jj
                ELSE
                    EXIT ind
                END IF
            ! ! ! se ne==0 e il carattere è 'E'(69) oppure 'e'(101) e esiste il carattere successivo
            ELSEIF(ne==0 .and. (IACHAR(stringa(jj:jj))==69 .or. IACHAR(stringa(jj:jj))==101) .and. jj+1<=LEN(stringa))THEN casi
            ! ! ! se il carattere successivo è una cifra
                IF(IACHAR(stringa(jj+1:jj+1))-47>=1 .and. IACHAR(stringa(jj+1:jj+1))-47<=10)THEN
                    ne=1
                    ind2(kk)=jj
                ELSE
                    EXIT ind
                END IF
            ! ! ! se ne==1 e il carattere precedente è 'E'(69) oppure 'e'(101) e
            ! ! ! il carattere è '+'(43) oppure '-'(45) e esiste il carattere dopo
            ELSEIF(ne==1 .and. (IACHAR(stringa(jj-1:jj-1))==69 .or. IACHAR(stringa(jj-1:jj-1))==101) .and. &
                  & jj+1<=LEN(stringa) .AND. (IACHAR(stringa(jj:jj))==43 .or. IACHAR(stringa(jj:jj))==45))THEN casi  ! + o -
            ! ! ! se il carattere successivo è una cifra
                IF(IACHAR(stringa(jj+1:jj+1))-47>=1 .and. IACHAR(stringa(jj+1:jj+1))-47<=10)THEN
                    ind2(kk)=jj
                ELSE
                    EXIT ind
                END IF
            ELSE casi
                EXIT ind
            END IF casi
        END DO ind

        IF(ind1(kk)>1)THEN
            IF(IACHAR(stringa(ind1(kk)-1:ind1(kk)-1))==43 .or. IACHAR(stringa(ind1(kk)-1:ind1(kk)-1))==45) ind1(kk)=ind1(kk)-1
        END IF

        READ(stringa(ind1(kk):ind2(kk)),*) num_r

    END IF

  END FUNCTION num_r






END MODULE StrNumConversion








! ! PROGRAM test
! !   USE StrNumConversion
! !  IMPLICIT NONE
! !   REAL::f
! !   INTEGER::i
! !   REAL(KIND=8)::f2
! !   CHARACTER(LEN=15)::stringa
! !
! !     i=132456789
! !     i=1324567890
! !     i=-999999999
! !     WRITE(*,*) str_i(i)
! !     i=i-1
! !     WRITE(*,*) i, str_i(i)
! !
! !     stringa='xxx12.345e01xxx'
! !     WRITE(*,*) num(stringa), num(stringa)+1
! !
! ! END PROGRAM test

