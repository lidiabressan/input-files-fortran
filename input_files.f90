!------------------------------------------------------------------------------
!
! MODULE: input_files
!
!> @author
!> Lidia Bressan
!
!  2014 05 16
!
! DESCRIPTION:
!>
!>  Module to handle input files.
!>
!>  It contains:
!>
!>
!>   FUNCTION contarighe: to count the lines of a file.
!>
!>   SUBROUTINE leggi_filels: to read a file containing a list of filenames.
!
!
!------------------------------------------------------------------------------

MODULE input_files
 IMPLICIT NONE

contains


  FUNCTION contarighe(u_in)
   !---------------------------------------------------------------------------
   ! DESCRIPTION:
   !>   FUNCTION contarighe (u_in)
   !> @brief
   !>   It counts lines of file unit=u_in.
   !
   !> @param[in] u_in
   !> @param[out] contarighe
   !> @return contarighe
   !---------------------------------------------------------------------------
   INTEGER,INTENT(IN)::u_in
   INTEGER::contarighe
   INTEGER::stato
   CHARACTER::riga

    contarighe=0
    REWIND(u_in)
    DO
        READ(u_in,*,IOSTAT=stato) riga
        IF(stato /= 0)THEN
            EXIT
        ELSE
            contarighe = contarighe+1
        END IF
    END DO
    REWIND(u_in)
  END FUNCTION contarighe




 SUBROUTINE leggi_filels( u_in, path_filels, nfile, nomefiles, &
                         & path_dati, nrighe, u_righe )
   !---------------------------------------------------------------------------
   ! DESCRIPTION:
   !> SUBROUTINE leggi_filels( u_in, path_filels, nfile, nomefiles,
   !>                          path_dati, nrighe, u_righe )
   !>
   !>   Read a file with a list of files (to analyze),
   !>   and optionally it counts the number of lines of each file.
   !
   !> @param[in] u_in:        unit of file
   !> @param[in] path_filels: file name containing a list of files.
   !> @param[in] nomefiles:   allocatable array that will contain the filenames.
   !> @param[in] u_righe:     unit of file to be used to count the lines of the files
   !>                         listed in path_filels.
   !> @param[in] path_dati:   path where you find the files listed in path_filels.
   !> @param[in] nrighe:      if present u_righe, allocatable array of integers to store
   !>                         the number of lines of each file.
   !>
   !> @param[out] nfile:      nbumber of files in path_filels.
   !> @param[out] nomefiles:  array of length nfile filled with the filenames in path_filels.
   !> @param[out] nrighe:     if present u_righe, array of size nfile with
   !>                         the number of lines of each file.
   !>
   !---------------------------------------------------------------------------
   !!! input
   INTEGER,INTENT(IN)::u_in
   CHARACTER(LEN=*),INTENT(IN)::path_filels
   CHARACTER(LEN=*),ALLOCATABLE,DIMENSION(:),INTENT(INOUT)::nomefiles
   !!!! per contare le righe dei file in path_filels
   INTEGER,INTENT(IN),OPTIONAL::u_righe
   CHARACTER(LEN=*),INTENT(IN),OPTIONAL::path_dati
   INTEGER,ALLOCATABLE,DIMENSION(:),INTENT(INOUT),OPTIONAL::nrighe
   !!! output
   INTEGER,INTENT(OUT)::nfile
   !!! output
   CHARACTER(LEN=LEN(nomefiles))::riga
   CHARACTER(LEN=140)::path_in
   INTEGER::stato, ii
   LOGICAL::conta_righe



    IF(PRESENT(nrighe) .AND. PRESENT(u_righe))THEN
        conta_righe = .true.
        IF(PRESENT(path_dati))THEN
            path_in=path_dati
        ELSE
            path_in=''
        END IF
    ELSE
        conta_righe = .false.
    END IF

! ! ! leggi il file di elenco dei file
    OPEN (UNIT=u_in, FILE=TRIM(path_filels), STATUS='OLD', ACTION='READ', IOSTAT=stato)
    IF (stato /= 0)    ERROR STOP 'leggi_filels: open path_filels'
    nfile=contarighe(u_in)

    ALLOCATE(nomefiles(nfile), STAT=stato)
    IF (stato /= 0)   ERROR STOP 'leggi_filels: allocate nomefiles'
    IF(conta_righe)THEN
        ALLOCATE(nrighe(nfile), STAT=stato)
        IF (stato /= 0)   ERROR STOP 'leggi_filels: allocate nrighe (optional)'
    END IF

    DO ii=1,nfile
        READ(u_in,*,IOSTAT=stato) riga
        IF(stato /= 0)THEN
            EXIT
        ELSE
            nomefiles(ii) = riga
            IF(conta_righe)THEN
                OPEN (UNIT=u_righe, FILE=TRIM(path_in)//TRIM(nomefiles(ii)), STATUS='OLD', ACTION='READ', IOSTAT=stato)
                IF (stato /= 0)    STOP 'leggi_filels: open file to count lines'
                nrighe(ii) = contarighe(u_righe)
                CLOSE(u_righe)
            END IF
        END IF
    END DO
    CLOSE(u_in)
    WRITE(*,*) 'Numero dei file da analizzare: ', nfile

  END SUBROUTINE leggi_filels


END MODULE input_files

