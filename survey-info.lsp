;==============================================================================
; GERADOR DE PALITOS DE SONDAGEM COM LEITURA DE DADOS CSV
;==============================================================================
; Descri��o: Cria palitos de sondagem no AutoCAD com dados extra�dos de arquivo CSV
; Autor: Andr� Buchmann M�ller
; Data de Cria��o: [Data]
; Vers�o: 1.0
;;
;; Funcionalidades:
;; - Leitura de dados de sondagem de arquivo CSV
;; - Cria��o autom�tica de palitos com bandeira informativa
;; - Inser��o de informa��es t�cnicas (cota, profundidade, proje��o, N.A.)
;; - Cria��o de layers espec�ficos para diferentes elementos
;; - Valida��o de dados de entrada
;;
;; Formato do arquivo CSV esperado:
;; Nome;Cota;Profundidade;Proje��o;N.A.
;; Exemplo: SOND-01;125,50;15,75;12,00;8,25
;;==============================================================================

(defun c:SONDM (/ *error* nome csv-data cota prof proj na2 p1 osn ml mt
                p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18
                p14a p19 p20 p21 p22 p23 p24 p25 p26 p27 p28 p29 p30 p30a
                p14b p15b p17a dist1 dist2 dist3)
  
  ;;----------------------------------------------------------------------------
  ;; FUN��O DE TRATAMENTO DE ERROS
  ;;----------------------------------------------------------------------------
  (defun *error* (msg)
    (if (not (member msg '("console break" "Function cancelled" "quit / exit abort")))
      (princ (strcat "\nERRO: " msg))
    )
    ;; Restaura vari�veis do sistema
    (if (boundp 'osn) (setvar "osmode" osn))
    (if (boundp 'ml) (setvar "clayer" ml))
    (if (boundp 'mt) (setvar "textstyle" mt))
    (princ)
  )
  
  ;;----------------------------------------------------------------------------
  ;; FUN��O: CONVERS�O DE FORMATO DECIMAL EUROPEU PARA AMERICANO
  ;;----------------------------------------------------------------------------
  ;; Converte n�meros com v�rgula (formato europeu) para ponto (formato americano)
  ;; Par�metros:
  ;;   str - String contendo n�mero decimal
  ;; Retorno:
  ;;   String com formato americano ou "0" se inv�lido
  ;;----------------------------------------------------------------------------
  (defun convert-decimal-format (str)
    (if (and str (> (strlen str) 0))
      (progn
        ;; Substitui v�rgula por ponto para n�meros decimais
        (setq str (vl-string-subst "." "," str))
        str
      )
      "0"
    )
  )

  ;;----------------------------------------------------------------------------
  ;; FUN��O: LEITURA DE DADOS DO ARQUIVO CSV
  ;;----------------------------------------------------------------------------
  ;; L� arquivo CSV e procura dados da sondagem especificada
  ;; Par�metros:
  ;;   nome-sondagem - Nome da sondagem a ser localizada
  ;; Retorno:
  ;;   Lista com dados (cota, profundidade, proje��o, N.A.) ou nil se n�o encontrado
  ;;----------------------------------------------------------------------------
  (defun read-csv-data (nome-sondagem / csv-path csv-file line-data fields found-data)
    (princ "\nIniciando leitura de dados do CSV...")
    
    ;; Valida��o do nome da sondagem
    (if (or (not nome-sondagem) (= nome-sondagem ""))
      (progn
        (princ "\nERRO: Nome da sondagem n�o pode ser vazio")
        (return nil)
      )
    )
    
    ;; Constr�i caminho do arquivo CSV (mesmo diret�rio do desenho)
    (setq csv-path (strcat (getvar "DWGPREFIX") "sondagens.csv"))
    (princ (strcat "\nProcurando arquivo: " csv-path))
    
    ;; Verifica se arquivo CSV existe
    (if (not (findfile csv-path))
      (progn
        (princ (strcat "\nERRO: Arquivo CSV n�o encontrado: " csv-path))
        (return nil)
      )
    )
    
    ;; Abre arquivo CSV para leitura
    (setq csv-file (open csv-path "r"))
    (if (not csv-file)
      (progn
        (princ (strcat "\nERRO: N�o foi poss�vel abrir o arquivo: " csv-path))
        (return nil)
      )
    )
    
    (princ "\nArquivo CSV aberto com sucesso")
    (princ (strcat "\nProcurando sondagem: " nome-sondagem))
    
    ;; Ignora linha de cabe�alho
    (read-line csv-file)
    
    ;; L� cada linha e procura pelo nome da sondagem
    (setq found-data nil)
    (while (and (setq line-data (read-line csv-file)) (not found-data))
      (if line-data
        (progn
          ;; Divide linha por ponto e v�rgula
          (setq fields (split-string line-data ";"))
          (if (>= (length fields) 5)
            (progn
              ;; Verifica se primeiro campo (nome da sondagem) corresponde
              (if (= (strcase (nth 0 fields)) (strcase nome-sondagem))
                (progn
                  (setq found-data (list
                    (nth 1 fields)  ; cota - coluna B
                    (atof (convert-decimal-format (nth 2 fields)))  ; profundidade - coluna C
                    (nth 3 fields)  ; proje��o - coluna D
                    (convert-decimal-format (nth 4 fields))  ; N.A. - coluna E
                  ))
                  (princ (strcat "\nSondagem encontrada: " (nth 0 fields)))
                )
              )
            )
          )
        )
      )
    )
    
    ;; Fecha arquivo CSV
    (close csv-file)
    
    ;; Retorna dados encontrados ou nil
    (if found-data
      (progn
        (princ "\nDados extra�dos com sucesso:")
        (princ (strcat "\nCota: " (nth 0 found-data)))
        (princ (strcat "\nProfundidade: " (rtos (nth 1 found-data))))
        (princ (strcat "\nProje��o: " (nth 2 found-data)))
        (princ (strcat "\nN.A.: " (nth 3 found-data)))
        found-data
      )
      (progn
        (princ (strcat "\nSondagem '" nome-sondagem "' n�o encontrada"))
        nil
      )
    )
  )
  
  ;;----------------------------------------------------------------------------
  ;; FUN��O: DIVIS�O DE STRING POR DELIMITADOR
  ;;----------------------------------------------------------------------------
  ;; Divide uma string usando um delimitador espec�fico
  ;; Par�metros:
  ;;   str - String a ser dividida
  ;;   delimiter - Caractere delimitador
  ;; Retorno:
  ;;   Lista com substrings resultantes
  ;;----------------------------------------------------------------------------
  (defun split-string (str delimiter / result pos)
    (setq result '())
    (while (setq pos (vl-string-search delimiter str))
      (setq result (append result (list (substr str 1 pos))))
      (setq str (substr str (+ pos 2)))
    )
    (setq result (append result (list str)))
    result
  )

  ;;============================================================================
  ;; IN�CIO DA FUN��O PRINCIPAL
  ;;============================================================================
  (princ "\n==============================================================================")
  (princ "\n                 GERADOR DE PALITOS DE SONDAGEM")
  (princ "\n==============================================================================")
  (princ "\nIniciando comando SONDM...")
  
  ;;----------------------------------------------------------------------------
  ;; ENTRADA DE DADOS DO USU�RIO
  ;;----------------------------------------------------------------------------
  ;; Solicita nome da sondagem
  (setq nome (getstring "\nDigite o nome da sondagem: "))
  
  ;; Valida entrada do usu�rio
  (if (or (not nome) (= nome ""))
    (progn
      (princ "\nERRO: Nome da sondagem n�o pode ser vazio")
      (exit)
    )
  )
  
  ;;----------------------------------------------------------------------------
  ;; LEITURA DOS DADOS DO CSV
  ;;----------------------------------------------------------------------------
  (princ "\nLendo dados do arquivo CSV...")
  (setq csv-data (read-csv-data nome))
  
  (if csv-data
    (progn
      (princ "\nProcessando dados...")
      (setq cota (nth 0 csv-data))      ; Cota da sondagem
      (setq prof (nth 1 csv-data))      ; Profundidade
      (setq proj (nth 2 csv-data))      ; Proje��o
      (setq na2 (nth 3 csv-data))       ; N�vel d'�gua
      
      ;; Valida��o dos dados extra�dos
      (if (not (numberp prof))
        (progn
          (princ "\nERRO: Profundidade deve ser um n�mero")
          (exit)
        )
      )
      
      (if (<= prof 0)
        (progn
          (princ "\nERRO: Profundidade deve ser maior que zero")
          (exit)
        )
      )
      
      ;; Exibe dados encontrados
      (princ (strcat "\nDados encontrados para " nome ":"))
      (princ (strcat "\nCota: " cota))
      (princ (strcat "\nProfundidade: " (rtos prof)))
      (princ (strcat "\nProje��o: " proj))
      (princ (strcat "\nN.A.: " na2))
    )
    (progn
      (princ (strcat "\nERRO: Sondagem " nome " n�o encontrada no arquivo CSV."))
      (exit)
    )
  )
  
  ;;----------------------------------------------------------------------------
  ;; SELE��O DO PONTO DE INSER��O
  ;;----------------------------------------------------------------------------
  (setq p1 (getpoint "\nClique o ponto de inser��o do palito: "))
  
  ;; Valida ponto selecionado
  (if (not p1)
    (progn
      (princ "\nERRO: Ponto de inser��o n�o selecionado")
      (exit)
    )
  )
  
  ;;----------------------------------------------------------------------------
  ;; CONFIGURA��O DO AMBIENTE DE DESENHO
  ;;----------------------------------------------------------------------------
  ;; Salva vari�veis atuais do sistema
  (setq osn (getvar "osmode"))          ; Modo de snap
  (setq ml (getvar "clayer"))           ; Layer atual
  (setq mt (getvar "textstyle"))        ; Estilo de texto
  (setvar "osmode" 0)                   ; Desativa snap durante desenho

  ;;============================================================================
  ;; CRIA��O DA BANDEIRA INFORMATIVA
  ;;============================================================================
  (princ "\nCriando bandeira informativa...")
  
  ;; C�lculo dos pontos da bandeira
  (setq p2 (polar p1 (* (/ 90.0 180.0) pi) 6.5))     ; Liga��o do palito com a bandeira
  (setq p3 (polar p2 (* (/ 180.0 180.0) pi) 6.75))   ; Ponto esquerdo baixo da bandeira
  (setq p4 (polar p3 (* (/ 90.0 180.0) pi) 5.0))     ; Ponto esquerdo alto da bandeira
  (setq p5 (polar p4 (* (/ 0.0 180.0) pi) 13.5))     ; Ponto direito alto da bandeira
  (setq p6 (polar p5 (* (/ 270.0 180.0) pi) 5.0))    ; Ponto direito baixo da bandeira
  (setq p7 (polar p4 (* (/ 270.0 180.0) pi) 2.0))    ; Ponto esquerdo da 1� divis�o
  (setq p8 (polar p7 (* (/ 0.0 180.0) pi) 13.5))     ; Ponto direito da 1� divis�o
  (setq p9 (polar p7 (* (/ 270.0 180.0) pi) 1.5))    ; Ponto esquerdo da 2� divis�o
  (setq p10 (polar p9 (* (/ 0.0 180.0) pi) 13.5))    ; Ponto direito da 2� divis�o
  
  ;; Pontos para posicionamento dos textos
  (setq p11 (polar p2 (* (/ 90.0 180.0) pi) 4.0))    ; Texto de identifica��o
  (setq p12 (polar p2 (* (/ 90.0 180.0) pi) 2.25))   ; Texto da cota
  (setq p13 (polar p2 (* (/ 90.0 180.0) pi) 0.75))   ; Texto da proje��o

  ;; Cria��o do layer para perfil/contorno
  (command "_layer" "m" "msp-ge_perfil" "c" "3" "msp-ge_perfil" "")
  
  ;; Desenho da estrutura da bandeira
  (command "_line" p1 p2 "")                          ; Liga��o bandeira-palito
  (command "_pline" p3 p4 p5 p6 "close")              ; Contorno da bandeira
  (command "_line" p7 p8 "")                          ; Primeira divis�o
  (command "_line" p9 p10 "")                         ; Segunda divis�o

  ;; Cria��o do layer para textos
  (command "_layer" "m" "msp-ge_textos" "c" "2" "msp-ge_textos" "")
  
  ;; Inser��o dos textos informativos
  (command "text" "j" "MC" p11 1.5 0 nome)                      ; Nome da sondagem
  (command "text" "j" "MC" p12 1.0 0 cota)                      ; Cota da sondagem
  (command "text" "j" "MC" p13 1.0 0 (strcat "Proj.: " proj "m")) ; Proje��o

  ;;============================================================================
  ;; CRIA��O DO PALITO DE SONDAGEM
  ;;============================================================================
  (princ "\nCriando palito de sondagem...")
  
  ;; C�lculo dos pontos do palito
  (setq p14 (polar p1 (* (/ 180.0 180.0) pi) 0.375))           ; Ponto esquerdo alto
  (setq p15 (polar p1 (* (/ 0.0 180.0) pi) 0.375))             ; Ponto direito alto
  (setq p16 (polar p14 (* (/ 270.0 180.0) pi) (* prof 2.5)))   ; Ponto esquerdo baixo
  (setq p17 (polar p15 (* (/ 270.0 180.0) pi) (* prof 2.5)))   ; Ponto direito baixo
  (setq p18 (polar p1 (* (/ 270.0 180.0) pi) (+ (* prof 2.5) 1.7))) ; Ponto do texto da profundidade
  
  ;; Desenho do palito
  (command "_layer" "m" "msp-ge_perfil" "c" "3" "msp-ge_perfil" "")
  (command "_pline" p14 p16 p17 p15 "close")          ; Contorno do palito
  
  ;; Inser��o do texto da profundidade
  (command "_layer" "m" "msp-ge_textos" "c" "2" "msp-ge_textos" "")
  (command "text" "j" "MC" p18 1.25 0 (rtos prof))   ; Comprimento da sondagem
  
  ;; Ponto de refer�ncia para c�lculos posteriores
  (setq p14a (polar p14 (* (/ 180.0 180.0) pi) 0))
 
  ;;============================================================================
  ;; CRIA��O DO INDICADOR DE N�VEL D'�GUA (N.A.)
  ;;============================================================================
  (princ "\nCriando indicador de n�vel d'�gua...")
  
  ;; Valida��o do valor do N.A.
  (if (not (numberp (atof na2)))
    (progn
      (princ "\nAVISO: Valor N.A. inv�lido, usando 0")
      (setq na2 "0")
    )
  )
  
  ;; C�lculo dos pontos do indicador N.A.
  (setq p19 (polar p14 (* (/ 180.0 180.0) pi) 1.5))           ; Ponto direito alto
  (setq p20 (polar p19 (* (/ 270.0 180.0) pi) (* 2.5 (atof na2)))) ; Ponto direito
  (setq p21 (polar p20 (* (/ 180.0 180.0) pi) 1.25))         ; Ponto do meio
  (setq p22 (polar p21 (* (/ 180.0 180.0) pi) 1.25))         ; Ponto esquerdo
  (setq p23 (polar p21 (* (/ 270.0 180.0) pi) 1.25))         ; Ponto abaixo
  
  ;; Inser��o do texto N.A.
  (command "text" "j" "BC" p21 1 0 "N.A.")
  
  ;; Cria��o do layer para N.A.
  (command "_layer" "m" "msp-ge_NA" "c" "1" "msp-ge_NA" "")
  
  ;; Desenho do indicador N.A.
  (command "_pline" p20 p22 p23 "close")              ; Contorno do N.A.
  (command "_solid" p20 p22 p23 p20 "")               ; Preenchimento s�lido

  ;;============================================================================
  ;; CRIA��O DE MARCA��ES E TEXTOS NO PALITO
  ;;============================================================================
  (princ "\nCriando marca��es no palito...")
  
  ;; C�lculo dos pontos iniciais para marca��es
  (setq p24 (polar p14 (* (/ 270.0 180.0) pi) 2.5))   ; Ponto baixo esquerdo do 1� s�lido
  (setq p25 (polar p24 (* (/ 0.0 180.0) pi) 0.75))    ; Ponto baixo direito do 1� s�lido
  (setq p26 (polar p24 (* (/ 270.0 180.0) pi) 2.5))   ; Ponto alto esquerdo do s�lido seguinte

  ;; C�lculo das dist�ncias para controle do loop
  (setq dist1 (distance p14 p16))                     ; Dist�ncia total do furo
  (setq dist2 (distance p14 p24))                     ; Dist�ncia do primeiro metro
  (setq dist3 (distance p14 p26))                     ; Dist�ncia alta do s�lido seguinte

  ;; Cria��o das marca��es ao longo do palito
  (if (<= dist1 dist2)
    ;; Se a profundidade � menor que um metro
    (progn
      (command "_layer" "m" "msp-ge_perfil" "c" "3" "msp-ge_perfil" "")
      (command "_solid" p14 p15 p16 p17 "")
      (setq p27 (polar p17 (* (/ 0.0 180.0) pi) 0.5))
      (command "_layer" "m" "msp-ge_textos" "c" "2" "msp-ge_textos" "")
      (command "text" "j" "l" p27 1 0 "-")            ; Marca��o de golpes
    )
    ;; Se a profundidade � maior que um metro, criar marca��es intervaladas
    (while (< dist3 dist1)
      (if (<= (+ dist3 2.5) dist1)
        (progn
          (setq p28 (polar p14 (* (/ 270.0 180.0) pi) 2.5))
          (setq p29 (polar p28 (* (/ 0.0 180.0) pi) 0.75))
          (setq p30 (polar p29 (* (/ 0.0 180.0) pi) 0.5))
          (command "_layer" "m" "msp-ge_perfil" "c" "3" "msp-ge_perfil" "")
          (command "_line" p28 p29 "")
          (command "_layer" "m" "msp-ge_textos" "c" "2" "msp-ge_textos" "")
          (command "text" "j" "l" p30 1 0 "-")        ; Marca��o de golpes
          (setq p14b (polar p28 (* (/ 270.0 180.0) pi) 2.5))
          
          (if (<= (+ dist3 2.5) dist1)
            (progn
              (setq p15b (polar p14b (* (/ 0.0 180.0) pi) 0.75))
              (setq p30a (polar p15b (* (/ 0.0 180.0) pi) 0.5))
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

  ;;============================================================================
  ;; FINALIZA��O
  ;;============================================================================
  ;; Restaura vari�veis do sistema
  (setvar "clayer" ml)
  (setvar "osmode" osn)
  (setvar "textstyle" mt)
  
  ;; Mensagem de conclus�o
  (princ "\n==============================================================================")
  (princ "\nPalito de sondagem criado com sucesso!")
  (princ (strcat "\nSondagem: " nome))
  (princ (strcat "\nCota: " cota))
  (princ (strcat "\nProfundidade: " (rtos prof) " m"))
  (princ (strcat "\nProje��o: " proj " m"))
  (princ (strcat "\nN.A.: " na2 " m"))
  (princ "\n==============================================================================")
  (princ)
)