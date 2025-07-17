;FAZ PALITO DE SONDAGEM COM LEITURA DE DADOS DO CSV

(defun c:SONDM(/ *error*)
  ;; Error handler function
  (defun *error* (msg)
    (if (not (member msg '("console break" "Function cancelled" "quit / exit abort")))
      (princ (strcat "\nERRO: " msg))
    )
    ;; Restore system variables
    (if (boundp 'osn) (setvar "osmode" osn))
    (if (boundp 'ml) (setvar "clayer" ml))
    (if (boundp 'mt) (setvar "textstyle" mt))
    (princ)
  )
  
  ;; Function to convert European decimal format (comma) to American format (dot)
  (defun convert-decimal-format (str)
    (if (and str (> (strlen str) 0))
      (progn
        ;; Replace comma with dot for decimal numbers
        (setq str (vl-string-subst "." "," str))
        str
      )
      "0"
    )
  )

  ;; Function to read CSV file and find survey data
  (defun read-csv-data (nome-sondagem / csv-path csv-file line-data fields found-data)
    (princ "\nIniciando leitura de dados do CSV...")
    
    (if (or (not nome-sondagem) (= nome-sondagem ""))
      (progn
        (princ "\nERRO: Nome da sondagem não pode ser vazio")
        (return nil)
      )
    )
    
    ;; Build CSV file path (same directory as the drawing)
    (setq csv-path (strcat (getvar "DWGPREFIX") "sondagens.csv"))
    (princ (strcat "\nProcurando arquivo: " csv-path))
    
    ;; Check if CSV file exists
    (if (not (findfile csv-path))
      (progn
        (princ (strcat "\nERRO: Arquivo CSV não encontrado: " csv-path))
        (return nil)
      )
    )
    
    ;; Open CSV file for reading
    (setq csv-file (open csv-path "r"))
    (if (not csv-file)
      (progn
        (princ (strcat "\nERRO: Não foi possível abrir o arquivo: " csv-path))
        (return nil)
      )
    )
    
    (princ "\nArquivo CSV aberto com sucesso")
    (princ (strcat "\nProcurando sondagem: " nome-sondagem))
    
    ;; Read header line (skip it)
    (read-line csv-file)
    
    ;; Read each line and search for the survey name
    (setq found-data nil)
    (while (and (setq line-data (read-line csv-file)) (not found-data))
      (if line-data
        (progn
          ;; Split line by semicolon
          (setq fields (split-string line-data ";"))
          (if (>= (length fields) 5)
            (progn
              ;; Check if first field (survey name) matches
              (if (= (strcase (nth 0 fields)) (strcase nome-sondagem))
                (progn
                  (setq found-data (list
                    (nth 1 fields)  ; cota - column B
                    (atof (convert-decimal-format (nth 2 fields)))  ; prof - column C (convert comma to dot)
                    (nth 3 fields)  ; proj - column D
                    (convert-decimal-format (nth 4 fields))  ; na2 - column E (convert comma to dot)
                  ))
                  (princ (strcat "\nSondagem encontrada: " (nth 0 fields)))
                )
              )
            )
          )
        )
      )
    )
    
    ;; Close CSV file
    (close csv-file)
    
    (if found-data
      (progn
        (princ "\nDados extraídos com sucesso:")
        (princ (strcat "\nCota: " (nth 0 found-data)))
        (princ (strcat "\nProfundidade: " (rtos (nth 1 found-data))))
        (princ (strcat "\nProjeção: " (nth 2 found-data)))
        (princ (strcat "\nNA: " (nth 3 found-data)))
        found-data
      )
      (progn
        (princ (strcat "\nSondagem '" nome-sondagem "' não encontrada"))
        nil
      )
    )
  )
  
  ;; Function to split string by delimiter
  (defun split-string (str delimiter / result pos)
    (setq result '())
    (while (setq pos (vl-string-search delimiter str))
      (setq result (append result (list (substr str 1 pos))))
      (setq str (substr str (+ pos 2)))
    )
    (setq result (append result (list str)))
    result
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
  
  ;; Read data from CSV
  (princ "\nLendo dados do CSV...")
  (setq csv-data (read-csv-data nome))
  
  (if csv-data
    (progn
      (princ "\nProcessando dados...")
      (setq cota (nth 0 csv-data))
      (setq prof (nth 1 csv-data))
      (setq proj (nth 2 csv-data))
      (setq na2 (nth 3 csv-data))
      
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
      (princ (strcat "\nERRO: Sondagem " nome " não encontrada no arquivo CSV."))
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
  (setq osn (getvar "osmode"))
  (setq ml (getvar "clayer"))
  (setq mt (getvar "textstyle"))
  (setvar "osmode" 0)

  ;FAZER A BANDEIRA
  (setq p2 (polar p1 (* (/ 90.0 180.0) pi) 6.5))    ;Cálculo da ligação do palito com a bandeira
  (setq p3 (polar p2 (* (/ 180.0 180.0) pi) 6.75))  ;Cálculo do ponto esquerdo baixo da bandeira
  (setq p4 (polar p3 (* (/ 90.0 180.0) pi) 5.0))    ;Cálculo do ponto esquerdo alto da bandeira
  (setq p5 (polar p4 (* (/ 0.0 180.0) pi) 13.5))    ;Cálculo do ponto direito alto da bandeira
  (setq p6 (polar p5 (* (/ 270.0 180.0) pi) 5.0))   ;Cálculo do ponto direito baixo da bandeira
  (setq p7 (polar p4 (* (/ 270.0 180.0) pi) 2.0))   ;Cálculo do ponto esquerdo da 1ª divisão da bandeira
  (setq p8 (polar p7 (* (/ 0.0 180.0) pi) 13.5))    ;Cálculo do ponto direito da 1ª divisão da bandeira
  (setq p9 (polar p7 (* (/ 270.0 180.0) pi) 1.5))   ;Cálculo do ponto esquerdo da 2ª divisão da bandeira
  (setq p10 (polar p9 (* (/ 0.0 180.0) pi) 13.5))   ;Cálculo do ponto direito da 2ª divisão da bandeira
  (setq p11 (polar p2 (* (/ 90.0 180.0) pi) 4.0))   ;Cálculo do texto de identificação da sondagem
  (setq p12 (polar p2 (* (/ 90.0 180.0) pi) 2.25))  ;Cálculo do texto da cota da sondagem
  (setq p13 (polar p2 (* (/ 90.0 180.0) pi) 0.75))  ;Cálculo do texto da projeção da sondagem

  (command "_layer" "m" "msp-ge_perfil" "c" "3" "msp-ge_perfil" "")    ;cria o layer do contorno da bandeira
  (command "_line" p1 p2 "")                                           ;desenha a ligação da bandeira com o palito
  (command "_pline" p3 p4 p5 p6 "close")                               ;desenha o contorno da bandeira
  (command "_line" p7 p8 "")                                           ;desenha a 1ª divisão da bandeira
  (command "_line" p9 p10 "")                                          ;desenha a 2ª divisão da bandeira

  (command "_layer" "m" "msp-ge_textos" "c" "2" "msp-ge_textos" "")    ;cria o layer dos textos
  (command "text" "j" "MC" p11 1.5 0 nome)                             ;nome da sondagem
  (command "text" "j" "MC" p12 1.0 0 cota)                             ;cota da sondagem
  (command "text" "j" "MC" p13 1.0 0 (strcat "Proj.: " proj "m"))      ;projeção da sondagem

  ;fazer o palito
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
 
  ;fazer N.A.
  ;; Validate na2 value before conversion
  (if (not (numberp (atof na2)))
    (progn
      (princ "\nAVISO: Valor N.A. inválido, usando 0")
      (setq na2 "0")
    )
  )
  
  (setq p19 (polar p14 (* (/ 180.0 180.0) pi) 1.5))        ;Cálculo do ponto direito alto do N.A.
  (setq p20 (polar p19 (* (/ 270.0 180.0) pi) (* 2.5 (atof na2))))  ;Cálculo do ponto direito do N.A.
  (setq p21 (polar p20 (* (/ 180.0 180.0) pi) 1.25))       ;Cálculo do ponto do meio do N.A.
  (setq p22 (polar p21 (* (/ 180.0 180.0) pi) 1.25))       ;Cálculo do ponto do esquerdo do N.A.
  (setq p23 (polar p21 (* (/ 270.0 180.0) pi) 1.25))       ;Cálculo do ponto abaixo do N.A.
  (command "text" "j" "BC" p21 1 0 "N.A.")        ;escreve o texto
  (command "_layer" "m" "msp-ge_NA" "c" "1" "msp-ge_NA" "")
  (command "_pline" p20 p22 p23 "close")                       ;desenha o contorno do NA
  (command "_solid" p20 p22 p23 p20 "")                        ;desenha o sólido

;colocar sólidos e textos no palito
  (setq p24 (polar p14 (* (/ 270.0 180.0) pi) 2.5))    ;cálculo do ponto baixo à esquerda do 1º sólido
  (setq p25 (polar p24 (* (/ 0.0 180.0) pi) 0.75))     ;cálculo do ponto baixo à direita do 1º sólido
  (setq p26 (polar p24 (* (/ 270.0 180.0) pi) 2.5))    ;cálculo do ponto alto à esquerda do sólido seguinte

  (setq dist1 (distance p14 p16))                              ;distância total do furo
  (setq dist2 (distance p14 p24))                              ;distância do 1º metro
  (setq dist3 (distance p14 p26))                              ;distância alta do sólido seguinte

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
          (setq p28 (polar p14 (* (/ 270.0 180.0) pi) 2.5))      ;cálculo do ponto abaixo à esquerda do 1º sólido
          (setq p29 (polar p28 (* (/ 0.0 180.0) pi) 0.75))      ;cálculo do ponto abaixo à direita do 1º sólido
          (setq p30 (polar p29 (* (/ 0.0 180.0) pi) 0.5))    ;Cálculo do ponto direito do texto
          (command "_layer" "m" "msp-ge_perfil" "c" "3" "msp-ge_perfil" "")
          (command "_line" p28 p29 "")
          (command "_layer" "m" "msp-ge_textos" "c" "2" "msp-ge_textos" "")
          (command "text" "j" "l" p30 1 0 "-")              ;coloca o texto dos golpes
          (setq p14b (polar p28 (* (/ 270.0 180.0) pi) 2.5))       
          (if (<= (+ dist3 2.5) dist1)
            (progn
              (setq p15b (polar p14b (* (/ 0.0 180.0) pi) 0.75))
              (setq p30a (polar p15b (* (/ 0.0 180.0) pi) 0.5))    ;Cálculo do ponto direito do texto
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

  (setvar "clayer" ml)
  (setvar "osmode" osn)
  (setvar "textstyle" mt)
  (princ "\nComando SONDM executado com sucesso!")
  (princ)
)









