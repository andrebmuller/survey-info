;FAZ PALITO DE SONDAGEM COM INFORMAÇÕES DE PERFIL
(defun c:SONDM(/ *error*)
  ;; Error handler function
  (defun *error* (msg)
    (if (not (member msg '("console break" "Function cancelled" "quit / exit abort")))
      (princ (strcat "\nERRO: " msg))
    )
    ;; Clean up Excel objects if they exist
    (if (and (boundp 'workbook) workbook)
      (progn
        (princ "\nLimpando objetos Excel...")
        (vlax-invoke-method workbook "Close" :vlax-false)
        (vlax-release-object workbook)
      )
    )
    (if (and (boundp 'excel-app) excel-app)
      (progn
        (vlax-invoke-method excel-app "Quit")
        (vlax-release-object excel-app)
      )
    )
    ;; Restore system variables
    (if (boundp 'osn) (setvar "osmode" osn))
    (if (boundp 'ml) (setvar "clayer" ml))
    (if (boundp 'mt) (setvar "textstyle" mt))
    (princ)
  )
  
  ;; Function to read Excel data
  (defun read-excel-data (nome-sondagem / excel-app workbook worksheet range-data row-count col-count i found-row cota-val prof-val proj-val na2-val excel-path)
    (princ "\nIniciando leitura de dados do Excel...")
    
    (if (or (not nome-sondagem) (= nome-sondagem ""))
      (progn
        (princ "\nERRO: Nome da sondagem não pode ser vazio")
        (return nil)
      )
    )
    
    (setq excel-path (strcat (getvar "DWGPREFIX") "sondagens.xlsx"))
    (princ (strcat "\nProcurando arquivo: " excel-path))
    
    ;; Check if Excel file exists
    (if (not (findfile excel-path))
      (progn
        (princ (strcat "\nERRO: Arquivo Excel não encontrado: " excel-path))
        (return nil)
      )
    )
    
    ;; Try to create Excel application
    (if (not (setq excel-app (vlax-get-or-create-object "Excel.Application")))
      (progn
        (princ "\nERRO: Não foi possível criar aplicação Excel")
        (return nil)
      )
    )
    
    (princ "\nAbrindo Excel...")
    
    ;; Configure Excel application
    ;;(vlax-put-property excel-app "Visible" :vlax-false)
    ;;(vla-put-visible excel :vlax-true)
    ;;(vlax-put-property excel 'ScreenUpdating :vlax-true)
    ;;(vlax-put-property excel 'DisplayAlerts :vlax-true)
    ;;(setq workBook (vl-catch-all-apply 'vla-open (list (vlax-get-property excel "WorkBooks") excel-path)))
    ;;(setq worksheet (vlax-get-property workbook 'ActiveSheet))
    
    ;; Try to open workbook
    (if (not (setq workbook (vlax-invoke-method (vlax-get-property excel-app 'Workbooks) 'Open excel-path)))
      (progn
        (princ "\nERRO: Não foi possível abrir o arquivo Excel")
        (vlax-invoke-method excel-app "Quit")
        (vlax-release-object excel-app)
        (return nil)
      )
    )
    
    (princ "\nArquivo Excel aberto com sucesso")
    
    ;; Try to get worksheet
    (if (not (setq worksheet (vlax-get-property workbook 'ActiveSheet)))
      (progn
        (princ "\nERRO: Não foi possível acessar a planilha")
        (vlax-invoke-method workbook "Close" :vlax-false)
        (vlax-invoke-method excel-app "Quit")
        (vlax-release-object workbook)
        (vlax-release-object excel-app)
        (return nil)
      )
    )
    
    ;; Get the used range
    (if (not (setq range-data (vlax-get-property worksheet "UsedRange")))
      (progn
        (princ "\nERRO: Não foi possível obter dados da planilha")
        (vlax-invoke-method workbook "Close" :vlax-false)
        (vlax-invoke-method excel-app "Quit")
        (vlax-release-object worksheet)
        (vlax-release-object workbook)
        (vlax-release-object excel-app)
        (return nil)
      )
    )

    ;; Get rows and columns collections, then their counts
    (setq rows-collection (vlax-get-property range-data "Rows"))
    (setq cols-collection (vlax-get-property range-data "Columns"))
    (setq row-count (vlax-get-property rows-collection "Count"))
    (setq col-count (vlax-get-property cols-collection "Count"))
    
    ;; Release the collection objects
    (vlax-release-object rows-collection)
    (vlax-release-object cols-collection)
    
    (princ (strcat "\nPlanilha possui " (itoa row-count) " linhas e " (itoa col-count) " colunas"))
    (princ (strcat "\nProcurando sondagem: " nome-sondagem))
    
    ;; Search for the survey name in column A
    (setq found-row nil)
    (setq i 1)
    (while (and (<= i row-count) (not found-row))
      (setq cell-obj (vlax-invoke-method worksheet 'Cells i 1))
      (setq cell-value (vlax-get-property cell-obj 'Value))
      (vlax-release-object cell-obj)

      (if (and cell-value 
         (= (strcase (if (numberp cell-value) 
                        (rtos cell-value 2 0) 
                        (vl-princ-to-string cell-value))) 
            (strcase nome-sondagem)))
      (setq found-row i)
      )
      (setq i (+ i 1))
    )
    
    ;; If found, get the values from columns B, C, D, E
    (if found-row
      (progn
        (setq cota-cell (vlax-invoke-method worksheet 'Cells found-row 2))
        (setq cota-val (vlax-get-property cota-cell 'Value))
        (vlax-release-object cota-cell)

        (setq prof-cell (vlax-invoke-method worksheet 'Cells found-row 3))
        (setq prof-val (vlax-get-property prof-cell 'Value))
        (vlax-release-object prof-cell)

        (setq proj-cell (vlax-invoke-method worksheet 'Cells found-row 4))
        (setq proj-val (vlax-get-property proj-cell 'Value))
        (vlax-release-object proj-cell)

        (setq na2-cell (vlax-invoke-method worksheet 'Cells found-row 5))
        (setq na2-val (vlax-get-property na2-cell 'Value))
        (vlax-release-object na2-cell)
      )
      (princ (strcat "\nSondagem '" nome-sondagem "' não encontrada"))
    )
    
    ;; Close Excel
    (princ "\nFechando Excel...")
    (vlax-invoke-method workbook "Close" :vlax-false)
    (vlax-invoke-method excel-app "Quit")
    (vlax-release-object worksheet)
    (vlax-release-object workbook)
    (vlax-release-object excel-app)
    
    ;; Return the values as a list
    (if found-row
      (progn
        (princ "\nDados extraídos com sucesso")
        (list (if cota-val (vl-princ-to-string cota-val) "")
              (if prof-val prof-val 0.0)
              (if proj-val (vl-princ-to-string proj-val) "")
              (if na2-val (vl-princ-to-string na2-val) "0"))
      )
      nil
    )
  )

  ;; Main function starts here
  (princ "\nIniciando comando SONDM...")
  
  ;; Get survey name from user
  (setq nome (getstring "\n Digite o nome da sondagem: "))
  
  ;; Validate user input
  (if (or (not nome) (= nome ""))
    (progn
      (princ "\nERRO: Nome da sondagem não pode ser vazio")
      (exit)
    )
  )
  
  ;; Read data from Excel
  (princ "\nLendo dados do Excel...")
  (setq excel-data (read-excel-data nome))
  
  (if excel-data
    (progn
      (princ "\nProcessando dados...")
      (setq cota (nth 0 excel-data))
      (setq prof (nth 1 excel-data))
      (setq proj (nth 2 excel-data))
      (setq na2 (nth 3 excel-data))
      
      ;; Validate data
      (if (not (numberp prof))
        (progn
          (princ "\nERRO: Profundidade deve ser um número")
          (exit)
        )
      )
      
      (if (<= prof 0)
        (progn
          (princ "\nERRO: Profundidade deve ser maior que zero")
          (exit)
        )
      )
      
      (princ (strcat "\nDados encontrados para " nome ":"))
      (princ (strcat "\nCota: " cota))
      (princ (strcat "\nProfundidade: " (rtos prof)))
      (princ (strcat "\nProjeção: " proj))
      (princ (strcat "\nN.A.: " na2))
    )
    (progn
      (princ (strcat "\nERRO: Sondagem " nome " não encontrada no arquivo Excel."))
      (exit)
    )
  )
  
  ;; Get insertion point
  (setq p1 (getpoint "\n Clique o ponto de inserção do palito: "))
  
  ;; Validate point
  (if (not p1)
    (progn
      (princ "\nERRO: Ponto de inserção não selecionado")
      (exit)
    )
  )
  
  ;; Save current system variables
  (princ "\nSalvando configurações do sistema...")
  (setq osn (getvar "osmode"))
  (setq ml (getvar "clayer"))
  (setq mt (getvar "textstyle"))
  (setvar "osmode" 0)

  ;; Drawing operations with error protection
  (princ "\nDesenhando bandeira...")
  
  ;FAZ A BANDEIRA
  (setq p2 (polar p1 (* (/ 90.0 180.0) pi) 6.5))          ;Cálculo da ligação do palito com a bandeira
  (setq p3 (polar p2 (* (/ 180.0 180.0) pi) 6.75))        ;Cálculo do ponto esquerdo baixo da bandeira
  (setq p4 (polar p3 (* (/ 90.0 180.0) pi) 5.0))          ;Cálculo do ponto esquerdo alto da bandeira
  (setq p5 (polar p4 (* (/ 0.0 180.0) pi) 13.5))          ;Cálculo do ponto direito alto da bandeira
  (setq p6 (polar p5 (* (/ 270.0 180.0) pi) 5.0))         ;Cálculo do ponto direito baixo da bandeira
  (setq p7 (polar p4 (* (/ 270.0 180.0) pi) 2.0))         ;Cálculo do ponto esquerdo da 1ª divisão da bandeira
  (setq p8 (polar p7 (* (/ 0.0 180.0) pi) 13.5))          ;Cálculo do ponto direito da 1ª divisão da bandeira
  (setq p9 (polar p7 (* (/ 270.0 180.0) pi) 1.5))         ;Cálculo do ponto esquerdo da 2ª divisão da bandeira
  (setq p10 (polar p9 (* (/ 0.0 180.0) pi) 13.5))         ;Cálculo do ponto direito da 2ª divisão da bandeira
  (setq p11 (polar p2 (* (/ 90.0 180.0) pi) 4.0))         ;Cálculo do texto de identificação da sondagem
  (setq p12 (polar p2 (* (/ 90.0 180.0) pi) 2.25))        ;Cálculo do texto da cota da sondagem
  (setq p13 (polar p2 (* (/ 90.0 180.0) pi) 0.75))        ;Cálculo do texto da projeção da sondagem

  (command "_layer" "m" "msp-ge_perfil" "c" "3" "msp-ge_perfil" "")    ;cria o layer do contorno da bandeira
  (command "_line" p1 p2 "")                                           ;desenha a ligação da bandeira com o palito
  (command "_pline" p3 p4 p5 p6 "close")                               ;desenha o contorno da bandeira
  (command "_line" p7 p8 "")                                           ;desenha a 1ª divisão da bandeira
  (command "_line" p9 p10 "")                                          ;desenha a 2ª divisão da bandeira

  (princ "\nAdicionando textos à bandeira...")
  (command "_layer" "m" "msp-ge_textos" "c" "2" "msp-ge_textos" "")    ;cria o layer dos textos
  (command "text" "j" "MC" p11 1.5 0 nome)                             ;nome da sondagem
  (command "text" "j" "MC" p12 1.0 0 cota)                             ;cota da sondagem
  (command "text" "j" "MC" p13 1.0 0 (strcat "Proj.: " proj "m"))      ;projeção da sondagem

  (princ "\nDesenhando palito...")
  
  ;FAZ O PALITO
  (setq p14 (polar p1 (* (/ 180.0 180.0) pi) 0.375))             ;Cálculo do ponto esquerdo alto do palito
  (setq p15 (polar p1 (* (/ 0.0 180.0) pi) 0.375))               ;Cálculo do ponto direito alto do palito
  (setq p16 (polar p14 (* (/ 270.0 180.0) pi) (* prof 2.5)))     ;Cálculo do ponto esquerdo baixo do palito
  (setq p17 (polar p15 (* (/ 270.0 180.0) pi) (* prof 2.5)))     ;Cálculo do ponto direito baixo do palito
  (setq p18 (polar p1 (* (/ 270.0 180.0) pi) (+ (* prof 2.5) 1.7)))   ;Cálculo do ponto do texto da profundidade do palito
  (command "_layer" "m" "msp-ge_perfil" "c" "3" "msp-ge_perfil" "")
  (command "_pline" p14 p16 p17 p15 "close")                          ;desenha o contorno do palito
  (command "_layer" "m" "msp-ge_textos" "c" "2" "msp-ge_textos" "")
  (command "text" "j" "MC" p18 1.25 0 (rtos prof))                    ;coloca o comprimento da sondagem no fundo
  (setq p14a (polar p14 (* (/ 180.0 180.0) pi) 0))                   ;Fixed: removed undefined hori variable

  (princ "\nDesenhando N.A...")
  
  ;FAZ N.A.
  ;; Validate na2 value before conversion
  (if (not (numberp (atof na2)))
    (progn
      (princ "\nAVISO: Valor N.A. inválido, usando 0")
      (setq na2 "0")
    )
  )
  
  (setq p19 (polar p14 (* (/ 180.0 180.0) pi) 1.5))                   ;Cálculo do ponto direito alto do N.A.
  (setq p20 (polar p19 (* (/ 270.0 180.0) pi) (* 2.5 (atof na2))))    ;Cálculo do ponto direito do N.A.
  (setq p21 (polar p20 (* (/ 180.0 180.0) pi) 1.25))                  ;Cálculo do ponto do meio do N.A.
  (setq p22 (polar p21 (* (/ 180.0 180.0) pi) 1.25))                  ;Cálculo do ponto do esquerdo do N.A.
  (setq p23 (polar p21 (* (/ 270.0 180.0) pi) 1.25))                  ;Cálculo do ponto abaixo do N.A.
  (command "text" "j" "BC" p21 1 0 "N.A.")                            ;escreve o texto
  (command "_layer" "m" "msp-ge_NA" "c" "1" "msp-ge_NA" "")
  (command "_pline" p20 p22 p23 "close")                              ;desenha o contorno do NA
  (command "_solid" p20 p22 p23 p20 "")                               ;desenha o sólido

  (princ "\nAdicionando detalhes ao palito...")
  
  ;COLOCA SÓLIDOS e TEXTOS NO PALITO
  (setq p24 (polar p14 (* (/ 270.0 180.0) pi) 2.5))    ;cálculo do ponto baixo à esquerda do 1º sólido
  (setq p25 (polar p24 (* (/ 0.0 180.0) pi) 0.75))     ;cálculo do ponto baixo à direita do 1º sólido
  (setq p26 (polar p24 (* (/ 270.0 180.0) pi) 2.5))    ;cálculo do ponto alto à esquerda do sólido seguinte

  (setq dist1 (distance p14 p16))  ;distância total do furo
  (setq dist2 (distance p14 p24))  ;distância do 1º metro
  (setq dist3 (distance p14 p26))  ;distância alta do sólido seguinte

  (if (<= dist1 dist2)
    (progn
      (command "_layer" "m" "msp-ge_perfil" "c" "3" "msp-ge_perfil" "")
      (command "_solid" p14 p15 p16 p17 "")
      (setq p27 (polar p17 (* (/ 0.0 180.0) pi) 0.5))                   ;Cálculo do ponto direito do texto
      (command "_layer" "m" "msp-ge_textos" "c" "2" "msp-ge_textos" "")
      (command "text" "j" "l" p27 1 0 "-")                              ;coloca o texto dos golpes
    )
    (while (< dist3 dist1)
      (if (<= (+ dist3 2.5) dist1)
        (progn
          (setq p28 (polar p14 (* (/ 270.0 180.0) pi) 2.5))                       ;cálculo do ponto abaixo à esquerda do 1º sólido
          (setq p29 (polar p28 (* (/ 0.0 180.0) pi) 0.75))                        ;cálculo do ponto abaixo à direita do 1º sólido
          (setq p30 (polar p29 (* (/ 0.0 180.0) pi) 0.5))                         ;Cálculo do ponto direito do texto
          (command "_layer" "m" "msp-ge_perfil" "c" "3" "msp-ge_perfil" "")
          (command "_line" p28 p29 "")
          (command "_layer" "m" "msp-ge_textos" "c" "2" "msp-ge_textos" "")
          (command "text" "j" "l" p30 1 0 "-")                                    ;coloca o texto dos golpes
          (setq p14b (polar p28 (* (/ 270.0 180.0) pi) 2.5))       
          (if (<= (+ dist3 2.5) dist1)
            (progn
              (setq p15b (polar p14b (* (/ 0.0 180.0) pi) 0.75))
              (setq p30a (polar p15b (* (/ 0.0 180.0) pi) 0.5))                     ;Cálculo do ponto direito do texto
              (command "text" "j" "l" p30a 1 0 "-")
              (command "_layer" "m" "msp-ge_perfil" "c" "3" "msp-ge_perfil" "")
              (command "_solid" p28 p29 p14 p15 "")
              (command "_line" p14b p15b "")
              (setq p14 (polar p14b (* (/ 180.0 180.0) pi) 0))
              (setq p15 (polar p15b (* (/ 180.0 180.0) pi) 0))
            )
            (setq p14 (polar p14b (* (/ 270.0 180.0) pi) 2.5))
          )
        )
        (progn
          (command "_layer" "m" "msp-ge_perfil" "c" "3" "msp-ge_perfil" "")
          (command "_solid" p14b p15b p16 p17 "")
          (setq p17a (polar p17 (* (/ 0.0 180.0) pi) 0.5))
          (command "_layer" "m" "msp-ge_textos" "c" "2" "msp-ge_textos" "")
          (command "text" "j" "l" p17a 1 0 "-")
          (setq p14 (polar p14b (* (/ 270.0 180.0) pi) 2.5))
          (setq dist3 (distance p14a p14))
        )
      )
      (setq dist3 (distance p14a p14))
    )
  )

  ;; Restore system variables
  (princ "\nRestaurando configurações do sistema...")
  (setvar "clayer" ml)
  (setvar "osmode" osn)
  (setvar "textstyle" mt)
  
  (princ "\nComando SONDM executado com sucesso!")
  (princ) 
)