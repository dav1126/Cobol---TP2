       IDENTIFICATION DIVISION.
           PROGRAM-ID. TP2-STPIERREDAVID.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FICHIER-IDX      ASSIGN TO "EMPLOYES.DAT"
                  ORGANIZATION         INDEXED
                  ACCESS MODE          SEQUENTIAL
                  RECORD KEY           EMPL-CODE
                  ALTERNATE RECORD KEY EMPL-NOM-PRENOM WITH DUPLICATES
                  ALTERNATE RECORD KEY EMPL-DATEEMB WITH DUPLICATES
                  FILE STATUS          W-STATUT-FICHIER.

       DATA DIVISION.
       FILE SECTION.
       FD  FICHIER-IDX.
       01  FICHE-PERSONNELLE-IDX.
           05  EMPL-CODE                 PIC X(6).
           05  EMPL-REGION               PIC 99.
           05  EMPL-SEXE                 PIC X.
           05  EMPL-NOM-PRENOM.
               10  EMPL-NOM              PIC X(20).
               10  EMPL-PRENOM           PIC X(15).
           05  EMPL-DATEEMB              PIC 9(8).
           05  EMPL-TAUX                 PIC 99V99.
           05  EMPL-NB-HEURES            PIC 9(3).

       WORKING-STORAGE SECTION.
      ***************************************************************
      *FILE STATUS
      ***************************************************************
       01  W-STATUT-FICHIER                 PIC 99.

      ***************************************************************
      *VARIABLES DU HEADER1
      ***************************************************************
       01  W-HEADER1.
           05  W-TEXT1                     PIC X(27)
                                           VALUE "La cie CRACK-INFO".
           05  W-TEXT2                     PIC X(43)
                                 VALUE "Interrogation des employe(e)s".
           05  W-DATE-AJD                  PIC 9999/99/99.

      ***************************************************************
      *VARIABLE DU HEADER2
      ***************************************************************
       01  W-HEADER2.
           05  W-FIL-ARI1                         PIC X(18).
           05  W-FIL-ARI2                         PIC X(30).

      ***************************************************************
      * VARIABLES DU MENU-SEXE
      ***************************************************************
       01  W-MENU-SEXE.
           05  W-MENU-SEXE-TITRE       VALUE "Menu choix du sexe".
           05  W-MENU-SEXE-CHOIX1      VALUE "1. Femmes seulement".
           05  W-MENU-SEXE-CHOIX2      VALUE "2. Hommes seulement".
           05  W-MENU-SEXE-CHOIX3      VALUE "3. Femmes et hommes".
           05  W-MENU-SEXE-CHOIX
                                   VALUE "Votre choix (1, 2, 3, Q):".

       01  W-CHOIX-MENU-SEXE.
           05  W-CHOIX-SEXE                PIC X   VALUE SPACE.
               88  FEMMES                  VALUE "1".
               88  HOMMES                  VALUE "2".
               88  FEMMES-HOMMES           VALUE "3".
               88  QUITTER                 VALUE "Q" "q".
               88  W-CHOIX-SEXE-VALIDE     VALUE "1" "2" "3" "Q" "q".

      ***************************************************************
      * VARIABLES DU MENU-CLE
      ***************************************************************
       01  W-MENU-CLE.
           05  W-MENU-CLE-TITRE       VALUE "Menu choix de la cle".
           05  W-MENU-CLE-CHOIX1      VALUE "1. Par numero d'employe".
           05  W-MENU-CLE-CHOIX2      VALUE "2. Par nom d'employe".
           05  W-MENU-CLE-CHOIX3      VALUE "3. Par date d'embauche".
           05  W-MENU-CLE-CHOIX       VALUE "Votre choix (1, 2, 3):".

       01  W-CHOIX-MENU-CLE.
           05  W-CHOIX-CLE                 PIC X   VALUE SPACE.
               88  NUMERO                  VALUE "1".
               88  NOM                     VALUE "2".
               88  DATEEMB                 VALUE "3".
               88  RETOUR                  VALUE SPACE.
               88  W-CHOIX-CLE-VALIDE      VALUE "1" "2" "3" SPACE.

      ****************************************************************
      * VARIABLES DU MENU-NO
      ****************************************************************
       01  W-MENU-NO-INPUT.
           05  W-MENU-NO-DEBUT             PIC AAAA99 VALUE SPACE.
           05  W-MENU-NO-FIN               PIC AAAA99 VALUE SPACE.

      ****************************************************************
      * VARIABLES DU MENU-NOM
      ****************************************************************
       01  W-MENU-NOM-INPUT.
           05  W-MENU-NOM-DEBUT             PIC X(20) VALUE SPACE.
           05  W-MENU-NOM-FIN               PIC X(20) VALUE SPACE.

      ****************************************************************
      * VARIABLES DU MENU-DATEEMB
      ****************************************************************
       01  W-MENU-DATEEMB-INPUT.
           05  W-MENU-DATEEMB-DEBUT             PIC 9999/99/99
                                                 VALUE 00000000.
           05  W-MENU-DATEEMB-FIN               PIC 9999/99/99
                                                 VALUE 00000000.
           05  W-MENU-DATEEMB-INPUT1            PIC 9999/99/99
                                                   VALUE 00000000.
           05  W-MENU-DATEEMB-INPUT2            PIC 9999/99/99
                                                   VALUE 00000000.

      ***************************************************************
      *VARIABLE DE MESSAGE D'ERREUR
      **************************************************************
       01  W-ERREUR                     PIC X(50)   VALUE SPACE.

      ****************************************************************
      *VARIABLES BOOLÉEENNES DE RECHERCHE
      ***************************************************************
       01  W-FIN-RECHERCHE.
           05  W-IND-FIN-RECHERCHE         PIC 9 VALUE 0.
               88 W-FIN-RECHERCHE          VALUE 1.
       01  W-CORRESPONDANCE-TROUVE         PIC 9 VALUE 0.

      *****************************************************************
      * CURSEUR DE L'ÉCRAN DE RÉSULTAT
      ****************************************************************
       01 W-RESULTAT-CURSEUR           PIC X(1) VALUE SPACE.

       SCREEN SECTION.
       01 HEADERS.
           05              BLANK SCREEN
                           FOREGROUND-COLOR 1
                           BACKGROUND-COLOR 7.
           05 HEADER1      LINE 1 COL 1
                           FOREGROUND-COLOR 7
                           BACKGROUND-COLOR 4
                           PIC X(80) FROM W-HEADER1.
           05              LINE 2 COL 1
                           BLANK LINE
                           BACKGROUND-COLOR 4.

           05  HEADER2     LINE 3 COL 1
                           FOREGROUND-COLOR 1
                           BACKGROUND-COLOR 8
                           PIC X(80) FROM W-HEADER2.

           05  FOOTER2     LINE 23 COL 1
                           BLANK LINE
                           BACKGROUND-COLOR 8
                           PIC X(80) FROM W-ERREUR.
           05              LINE 24 COL 1
                           BLANK LINE
                           BACKGROUND-COLOR 4.
           05              LINE 25 COL 1
                           BLANK LINE
                           FOREGROUND-COLOR 7
                           BACKGROUND-COLOR 4.

       01  MENU-SEXE.
           05  MENU-SEXE-TITRE  LINE 5 COL 31
                           PIC X(40) FROM W-MENU-SEXE-TITRE.
           05  MENU-SEXE-CHOIX1 LINE 8 COL 28
                           PIC X(50) FROM W-MENU-SEXE-CHOIX1.
           05  MENU-SEXE-CHOIX2 LINE 9 COL 28
                           PIC X(40) FROM W-MENU-SEXE-CHOIX2.
           05  MENU-SEXE-CHOIX3 LINE 10 COL 28
                           PIC X(40) FROM W-MENU-SEXE-CHOIX3.
           05  MENU-SEXE-CHOIX  LINE 13 COL 28
                           PIC X(25) FROM W-MENU-SEXE-CHOIX.
           05  CHOIX-QUITTER   LINE 11 COL 28
                           VALUE "Q. Quitter".
           05  CHOIX-SEXE        LINE 13 COL 54
                           PIC X.


       01  MENU-CLE.
           05  MENU-CLE-TITRE  LINE 5 COL 31
                           PIC X(40) FROM W-MENU-CLE-TITRE.
           05  MENU-CLE-CHOIX1 LINE 8 COL 28
                           PIC X(50) FROM W-MENU-CLE-CHOIX1.
           05  MENU-CLE-CHOIX2 LINE 9 COL 28
                           PIC X(40) FROM W-MENU-CLE-CHOIX2.
           05  MENU-CLE-CHOIX3 LINE 10 COL 28
                           PIC X(40) FROM W-MENU-CLE-CHOIX3.
           05  MENU-CHOIX  LINE 13 COL 28
                           PIC X(25) FROM W-MENU-CLE-CHOIX.
           05  CHOIX-CLE        LINE 13 COL 50
                           PIC X.

       01  MENU-NO.
           05  MENU-NO-TITRE   LINE 5 COL 20
                VALUE "Entrez l'intervalle des numeros d'employes:".
           05  MENU-NO-INPUT1  LINE 8 COL 30
                VALUE "De :".
           05  MENU-NO-INPUT2  LINE 9 COL 30
                VALUE "A :".

       01  MENU-NOM.
           05  MENU-NOM-TITRE   LINE 5 COL 20
                VALUE "Entrez l'intervalle des noms de famille:".
           05  MENU-NOM-INPUT1  LINE 8 COL 30
                VALUE "De :".
           05  MENU-NOM-INPUT2  LINE 9 COL 30
                VALUE "A :".

       01 MENU-DATEEMB.
           05  MENU-DATEEMB-TITRE   LINE 5 COL 20
                VALUE "Entrez l'intervalle des dates d'embauche:".
           05  MENU-DATEEMB-INPUT1  LINE 8 COL 30
                VALUE "De :".
           05  MENU-DATEEMB-INPUT2  LINE 9 COL 30
                VALUE "A :".
           05 MENU-DATEEMB-INPUT11  LINE 8 COL 35
               PIC 9999/99/99 FROM W-MENU-DATEEMB-INPUT1.
           05 MENU-DATEEMB-INPUT22  LINE 9 COL 35
               PIC 9999/99/99 FROM W-MENU-DATEEMB-INPUT2.

       01  ECRAN-RESULTAT.
           05  ECRAN-RESULTAT-TITRE        LINE 5 COL 31
                   VALUE "Details de l'employe".
           05  ECRAN-RESULTAT-NO-1     LINE 7 COL 23
                   VALUE "Code:".
           05  ECRAN-RESULTAT-NO2           LINE 7 COL 51
                PIC X(6)   FROM  EMPL-CODE.
           05  ECRAN-RESULTAT-NOM1          LINE 8 COL 23
                   VALUE "Nom:".
           05  ECRAN-RESULTAT-NOM2          LINE 8 COL 51
                PIC X(20)  FROM EMPL-NOM.
           05  ECRAN-RESULTAT-PRENOM1       LINE 9 COL 23
                   VALUE "Prenom:".
           05  ECRAN-RESULTAT-PRENOM2       LINE 9 COL 51
                PIC X(15)  FROM EMPL-PRENOM.
           05  ECRAN-RESULTAT-SEXE1         LINE 10 COL 23
                   VALUE "Sexe:".
           05  ECRAN-RESULTAT-SEXE2         LINE 10 COL 51
                PIC X      FROM EMPL-SEXE.
           05  ECRAN-RESULTAT-REGION1       LINE 11 COL 23
                   VALUE "Region:".
           05  ECRAN-RESULTAT-REGION2       LINE 11 COL 51
                PIC 99     FROM EMPL-REGION.
           05  ECRAN-RESULTAT-TAUX1         LINE 12 COL 23
                   VALUE "Taux horaire:".
           05  ECRAN-RESULTAT-TAUX2         LINE 12 COL 51
                PIC $99.99  FROM EMPL-TAUX.
           05  ECRAN-RESULTAT-HEURE1        LINE 13 COL 23
                   VALUE "Nombre d'heures:".
           05  ECRAN-RESULTAT-HEURE2        LINE 13 COL 51
                PIC ZZZ   FROM EMPL-NB-HEURES.
           05  ECRAN-RESULTAT-DATEEMB1      LINE 14 COL 23
                   VALUE "Date d'embauche:".
           05  ECRAN-RESULTAT-DATEEMB2      LINE 14 COL 51
                PIC 9999/99/99  FROM EMPL-DATEEMB.
           05  ECRAN-RESULTAT-MSG          LINE 20 COL 15
                   VALUE "Appuyer sur ENTER pour continuer:".
           05  ECRAN-RESULTAT-CURSEUR      LINE 20 COL 49
                PIC X(1)   TO W-RESULTAT-CURSEUR.

       PROCEDURE DIVISION.
       00000-PRINCIPAL.
           OPEN INPUT FICHIER-IDX.
           MOVE FUNCTION CURRENT-DATE(1:8) TO W-DATE-AJD.
           PERFORM 10000-DISPLAY-HEADERS.
           PERFORM 20000-MENU-SEXE.

           CLOSE FICHIER-IDX.
           STOP RUN.

      ******************************************************************
       10000-DISPLAY-HEADERS.
           DISPLAY HEADERS.

      ******************************************************************
       20000-MENU-SEXE.
           MOVE SPACE TO W-FIL-ARI1.
           MOVE SPACE TO W-FIL-ARI2.
           MOVE SPACE TO W-CHOIX-SEXE.
           MOVE SPACE TO W-CHOIX-CLE.
           IF W-IND-FIN-RECHERCHE = 1
               CLOSE FICHIER-IDX
               OPEN INPUT FICHIER-IDX
           END-IF.

           PERFORM 10000-DISPLAY-HEADERS.

           DISPLAY MENU-SEXE.
           ACCEPT W-CHOIX-SEXE.
           IF NOT W-CHOIX-SEXE-VALIDE
             MOVE "CHOIX INVALIDE" TO W-ERREUR
             PERFORM 20000-MENU-SEXE
           ELSE
               MOVE SPACE TO W-ERREUR
           END-IF.

           EVALUATE TRUE
               WHEN FEMMES
                   MOVE "<Femmes seulement>" to W-FIL-ARI1
               WHEN HOMMES
                   MOVE "<Hommes seulement>" to W-FIL-ARI1
               WHEN FEMMES-HOMMES
                   MOVE "<Femmes et hommes>" TO W-FIL-ARI1
               WHEN QUITTER
                   PERFORM 10000-DISPLAY-HEADERS
                   DISPLAY "FIN DU PROGRAMME"
                   STOP RUN
           END-EVALUATE.

           PERFORM 30000-MENU-CLE.

      ******************************************************************
       30000-MENU-CLE.
           MOVE SPACE TO W-FIL-ARI2.
           MOVE SPACE TO W-CHOIX-CLE.
           PERFORM 10000-DISPLAY-HEADERS.

           DISPLAY MENU-CLE.
           ACCEPT W-CHOIX-CLE.
           IF NOT W-CHOIX-CLE-VALIDE
               MOVE "CHOIX INVALIDE" TO W-ERREUR
               PERFORM 10000-DISPLAY-HEADERS
               PERFORM 30000-MENU-CLE
           ELSE
               MOVE SPACE TO W-ERREUR
           END-IF.

           EVALUATE TRUE
               WHEN NUMERO
                   MOVE "<Numero d'employe>" TO W-FIL-ARI2
                   PERFORM 40000-MENU-NO
               WHEN NOM
                   MOVE "<Nom prenom>" TO W-FIL-ARI2
                   PERFORM 50000-MENU-NOM
               WHEN DATEEMB
                   MOVE "<Date d'embauche>" TO W-FIL-ARI2
                   PERFORM 60000-MENU-DATEEMB
               WHEN RETOUR
                   PERFORM 20000-MENU-SEXE
               END-EVALUATE.

      *****************************************************************
       40000-MENU-NO.
           MOVE SPACE TO W-MENU-NO-DEBUT.
           MOVE SPACE TO W-MENU-NO-FIN.
           MOVE 0 TO W-CORRESPONDANCE-TROUVE.
           MOVE 0 TO W-IND-FIN-RECHERCHE.

           PERFORM 10000-DISPLAY-HEADERS.
           DISPLAY MENU-NO.
           ACCEPT W-MENU-NO-DEBUT AT 0835.
           INSPECT W-MENU-NO-DEBUT CONVERTING
           "abcdefghijklmnopqrstuvwxyz" to "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
           ACCEPT W-MENU-NO-FIN AT 0935.
           INSPECT W-MENU-NO-FIN CONVERTING
           "abcdefghijklmnopqrstuvwxyz" to "ABCDEFGHIJKLMNOPQRSTUVWXYZ".

           EVALUATE TRUE
               WHEN W-MENU-NO-DEBUT > W-MENU-NO-FIN
                   MOVE "Cle invalide" TO W-ERREUR
                   PERFORM 40000-MENU-NO

               WHEN W-MENU-NO-DEBUT <> SPACE AND W-MENU-NO-FIN = SPACE
                   MOVE "ZZZ99" TO W-MENU-NO-FIN

               WHEN W-MENU-NO-DEBUT = SPACE AND W-MENU-NO-FIN <> SPACE
                   MOVE "A" TO W-MENU-NO-DEBUT

               WHEN W-MENU-NO-DEBUT = SPACE AND W-MENU-NO-FIN = SPACE
                   PERFORM 20000-MENU-SEXE
           END-EVALUATE.

      *    MOVE W-MENU-NO-DEBUT TO EMPL-CODE
      *    START FICHIER-IDX KEY >= EMPL-CODE
      *    INVALID KEY DISPLAY "ERROR".

           PERFORM 70000-RECHERCHE
               UNTIL W-IND-FIN-RECHERCHE = 1.

           IF W-CORRESPONDANCE-TROUVE = 0
               MOVE "Aucun employe trouve" TO W-ERREUR
               CLOSE FICHIER-IDX
               OPEN INPUT FICHIER-IDX
               PERFORM 40000-MENU-NO
           END-IF.

           MOVE SPACE TO W-ERREUR.
           PERFORM 20000-MENU-SEXE.

      *****************************************************************
       50000-MENU-NOM.
           MOVE SPACE TO W-MENU-NOM-DEBUT.
           MOVE SPACE TO W-MENU-NOM-FIN.
           MOVE 0 TO W-CORRESPONDANCE-TROUVE.
           MOVE 0 TO W-IND-FIN-RECHERCHE.

           PERFORM 10000-DISPLAY-HEADERS.
           DISPLAY MENU-NOM.
           ACCEPT W-MENU-NOM-DEBUT AT 0835.
           INSPECT W-MENU-NOM-DEBUT CONVERTING
           "abcdefghijklmnopqrstuvwxyz" to "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
           ACCEPT W-MENU-NOM-FIN AT 0935.
           INSPECT W-MENU-NOM-FIN CONVERTING
           "abcdefghijklmnopqrstuvwxyz" to "ABCDEFGHIJKLMNOPQRSTUVWXYZ".

           EVALUATE TRUE
              WHEN W-MENU-NOM-DEBUT > W-MENU-NOM-FIN
                   MOVE "Cle invalide" TO W-ERREUR
                   PERFORM 50000-MENU-NOM

               WHEN W-MENU-NOM-DEBUT <> SPACE AND W-MENU-NOM-FIN = SPACE
                   MOVE "ZZZZZZZZZZZZZZZZZZZZ" TO W-MENU-NO-FIN

               WHEN W-MENU-NOM-DEBUT = SPACE AND W-MENU-NOM-FIN <> SPACE
                   MOVE "A" TO W-MENU-NO-DEBUT

               WHEN W-MENU-NOM-DEBUT = SPACE AND W-MENU-NOM-FIN = SPACE
                   PERFORM 20000-MENU-SEXE
           END-EVALUATE.

           PERFORM 70000-RECHERCHE
               UNTIL W-IND-FIN-RECHERCHE = 1.

           IF W-CORRESPONDANCE-TROUVE = 0
               MOVE "Aucun employe trouve" TO W-ERREUR
               CLOSE FICHIER-IDX
               OPEN INPUT FICHIER-IDX
               PERFORM 50000-MENU-NOM
           END-IF.

           MOVE SPACE TO W-ERREUR.
           PERFORM 20000-MENU-SEXE.

      *****************************************************************
       60000-MENU-DATEEMB.
           MOVE 00000000 TO W-MENU-DATEEMB-DEBUT.
           MOVE 00000000 TO W-MENU-DATEEMB-FIN.
           MOVE 0 TO W-CORRESPONDANCE-TROUVE.
           MOVE 0 TO W-IND-FIN-RECHERCHE.

           PERFORM 10000-DISPLAY-HEADERS.
           DISPLAY MENU-DATEEMB.
           ACCEPT W-MENU-DATEEMB-DEBUT AT 0835.
           ACCEPT W-MENU-DATEEMB-FIN AT 0935.

           EVALUATE TRUE
              WHEN W-MENU-DATEEMB-DEBUT > W-MENU-DATEEMB-FIN
                   MOVE "Cle invalide" TO W-ERREUR
                   PERFORM 60000-MENU-DATEEMB

               WHEN W-MENU-DATEEMB-DEBUT <> 00000000
               AND W-MENU-DATEEMB-FIN = 00000000
                   MOVE FUNCTION CURRENT-DATE(1:8) TO W-MENU-NO-FIN

               WHEN W-MENU-DATEEMB-DEBUT = 00000000
               AND W-MENU-DATEEMB-FIN <> 00000000
                   MOVE "00000000" TO W-MENU-NO-DEBUT

               WHEN W-MENU-NOM-DEBUT = 00000000
               AND W-MENU-NOM-FIN = 00000000
                   PERFORM 20000-MENU-SEXE
           END-EVALUATE.

           PERFORM 70000-RECHERCHE
               UNTIL W-IND-FIN-RECHERCHE = 1.

           IF W-CORRESPONDANCE-TROUVE = 0
               MOVE "Aucun employe trouve" TO W-ERREUR
               CLOSE FICHIER-IDX
               OPEN INPUT FICHIER-IDX
               PERFORM 50000-MENU-NOM
           END-IF.

           MOVE SPACE TO W-ERREUR.
           PERFORM 20000-MENU-SEXE.

      *****************************************************************
       70000-RECHERCHE.
           READ FICHIER-IDX
               AT END MOVE 1 TO W-IND-FIN-RECHERCHE.
           DISPLAY W-MENU-NO-DEBUT.
           EVALUATE TRUE
               WHEN FEMMES AND NUMERO
                   IF (EMPL-SEXE = 'F'
                    AND EMPL-CODE >= W-MENU-NO-DEBUT
                    AND EMPL-CODE <= W-MENU-NO-FIN)
                       MOVE 1 TO W-CORRESPONDANCE-TROUVE
                       PERFORM 80000-AFFICHAGE-RESULTAT
                   END-IF
               WHEN HOMMES AND NUMERO
                   IF (EMPL-SEXE = 'M'
                    AND EMPL-CODE >= W-MENU-NO-DEBUT
                    AND EMPL-CODE <= W-MENU-NO-FIN)
                       MOVE 1 TO W-CORRESPONDANCE-TROUVE
                       PERFORM 80000-AFFICHAGE-RESULTAT
                   END-IF
               WHEN FEMMES-HOMMES AND NUMERO
                   IF (
                    EMPL-CODE >= W-MENU-NO-DEBUT
                    AND EMPL-CODE <= W-MENU-NO-FIN)
                       MOVE 1 TO W-CORRESPONDANCE-TROUVE
                       PERFORM 80000-AFFICHAGE-RESULTAT
                   END-IF

               WHEN FEMMES AND NOM
                   IF (EMPL-SEXE = 'F'
                    AND EMPL-NOM >= W-MENU-NOM-DEBUT
                    AND EMPL-NOM <= W-MENU-NOM-FIN)
                       MOVE 1 TO W-CORRESPONDANCE-TROUVE
                       PERFORM 80000-AFFICHAGE-RESULTAT
                   END-IF
               WHEN HOMMES AND NOM
                   IF (EMPL-SEXE = 'M'
                    AND EMPL-NOM >= W-MENU-NOM-DEBUT
                    AND EMPL-NOM <= W-MENU-NOM-FIN)
                       MOVE 1 TO W-CORRESPONDANCE-TROUVE
                       PERFORM 80000-AFFICHAGE-RESULTAT
                   END-IF
                WHEN FEMMES-HOMMES AND NOM
                    IF (
                     EMPL-NOM >= W-MENU-NOM-DEBUT
                     AND EMPL-NOM <= W-MENU-NOM-FIN)
                        MOVE 1 TO W-CORRESPONDANCE-TROUVE
                        PERFORM 80000-AFFICHAGE-RESULTAT
                    END-IF
                WHEN FEMMES AND DATEEMB
                   IF (EMPL-SEXE = 'F'
                       AND EMPL-DATEEMB >= W-MENU-DATEEMB-DEBUT
                       AND EMPL-DATEEMB <= W-MENU-DATEEMB-FIN)
                       MOVE 1 TO W-CORRESPONDANCE-TROUVE
                       PERFORM 80000-AFFICHAGE-RESULTAT
                   END-IF
                WHEN HOMMES AND DATEEMB
                   IF (EMPL-SEXE = 'M'
                       AND EMPL-DATEEMB >= W-MENU-DATEEMB-DEBUT
                       AND EMPL-DATEEMB <= W-MENU-DATEEMB-FIN)
                       MOVE 1 TO W-CORRESPONDANCE-TROUVE
                       PERFORM 80000-AFFICHAGE-RESULTAT
                   END-IF
                WHEN FEMMES-HOMMES AND DATEEMB
                   IF (
                       EMPL-DATEEMB >= W-MENU-DATEEMB-DEBUT
                       AND EMPL-DATEEMB <= W-MENU-DATEEMB-FIN)
                       MOVE 1 TO W-CORRESPONDANCE-TROUVE
                       PERFORM 80000-AFFICHAGE-RESULTAT
                   END-IF
           END-EVALUATE.

      *****************************************************************
       80000-AFFICHAGE-RESULTAT.
             PERFORM 10000-DISPLAY-HEADERS.
             DISPLAY ECRAN-RESULTAT.
             ACCEPT ECRAN-RESULTAT.

