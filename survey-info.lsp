;==============================================================================
; GERADOR DE PALITOS DE SONDAGEM COM LEITURA DE DADOS CSV
;==============================================================================
; Descrição: Cria palitos de sondagem no AutoCAD com dados extraídos de arquivo CSV
; Autor: André Buchmann Müller
; Data de Criação: [Data]
; Versão: 1.0
;;
;; Funcionalidades:
;; - Leitura de dados de sondagem de arquivo CSV
;; - Criação automática de palitos com bandeira informativa
;; - Inserção de informações técnicas (cota, profundidade, projeção, N.A.)
;; - Criação de layers específicos para diferentes elementos
;; - Validação de dados de entrada
;;
;; Formato do arquivo CSV esperado:
;; Nome;Cota;Profundidade;Projeção;N.A.
;; Exemplo: SOND-01;125,50;15,75;12,00;8,25
;;==============================================================================

(defun c:SONDM (/ *error* nome csv-data cota prof proj na2 p1 osn ml mt
                p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18
                p14a p19 p20 p21 p22 p23 p24 p25 p26 p27 p28 p29 p30 p30a
                p14b p15b p17a dist1 dist2 dist3)
  
  ;;----------------------------------------------------------------------------
  ;; FUNÇÃO DE TRATAMENTO DE ERROS
  ;;----------------------------------------------------------------------------
  (defun *error* (msg)
    (if (not (member msg '("console break" "Function cancelled" "quit / exit abort")))
      (princ (strcat "\nERRO: " msg))
    )
    ;; Restaura variáveis do sistema
    (if (boundp 'osn) (setvar "osmode" osn))
    (if (boundp 'ml) (setvar "clayer" ml))
    (if (boundp 'mt) (setvar "textstyle" mt))
    (princ)
  )
  
  ;;----------------------------------------------------------------------------
  ;; FUNÇÃO: CONVERSÃO DE FORMATO DECIMAL EUROPEU PARA AMERICANO
  ;;----------------------------------------------------------------------------
  ;; Converte números com vírgula (formato europeu) para ponto (formato americano)
  ;; Parâmetros:
  ;;   str - String contendo número decimal
  ;; Retorno:
  ;;   String com formato americano ou "0" se inválido
  ;;----------------------------------------------------------------------------
  (defun convert-decimal-format (str)
    (if (and str (> (strlen str) 0))
      (progn
        ;; Substitui vírgula por ponto para números decimais
        (setq str (vl-string-subst "." "," str))
        str
      )
      "0"
    )
  )

  ;;----------------------------------------------------------------------------
  ;; FUNÇÃO: LEITURA DE DADOS DO ARQUIVO CSV
  ;;----------------------------------------------------------------------------
  ;; Lê arquivo CSV e procura dados da sondagem especificada
  ;; Parâmetros:
  ;;   nome-sondagem - Nome da sondagem a ser localizada
  ;; Retorno:
  ;;   Lista com dados (cota, profundidade, projeção, N.A.) ou nil se não encontrado
  ;;----------------------------------------------------------------------------
  (defun read-csv-data (nome-sondagem / csv-path csv-file line-data fields found-data)
    (princ "\nIniciando leitura de dados do CSV...")
    
    ;; Validação do nome da sondagem
    (if (or (not nome-sondagem) (= nome-sondagem ""))
      (progn
        (princ "\nERRO: Nome da sondagem não pode ser vazio")
        (return nil)
      )
    )
    
    ;; Constrói caminho do arquivo CSV (mesmo diretório do desenho)
    (setq csv-path (strcat (getvar "DWGPREFIX") "sondagens.csv"))
    (princ (strcat "\nProcurando arquivo: " csv-path))
    
    ;; Verifica se arquivo CSV existe
    (if (not (findfile csv-path))
      (progn
        (princ (strcat "\nERRO: Arquivo CSV não encontrado: " csv-path))
        (return nil)
      )
    )
    
    ;; Abre arquivo CSV para leitura
    (setq csv-file (open csv-path "r"))
    (if (not csv-file)
      (progn
        (princ (strcat "\nERRO: Não foi possível abrir o arquivo: " csv-path))
        (return nil)
      )
    )
    
    (princ "\nArquivo CSV aberto com sucesso")
    (princ (strcat "\nProcurando sondagem: " nome-sondagem))
    
    ;; Ignora linha de cabeçalho
    (read-line csv-file)
    
    ;; Lê cada linha e procura pelo nome da sondagem
    (setq found-data nil)
    (while (and (setq line-data (read-line csv-file)) (not found-data))
      (if line-data
        (progn
          ;; Divide linha por ponto e vírgula
          (setq fields (split-string line-data ";"))
          (if (>= (length fields) 5)
            (progn
              ;; Verifica se primeiro campo (nome da sondagem) corresponde
              (if (= (strcase (nth 0 fields)) (strcase nome-sondagem))
                (progn
                  (setq found-data (list
                    (nth 1 fields)  ; cota - coluna B
                    (atof (convert-decimal-format (nth 2 fields)))  ; profundidade - coluna C
                    (nth 3 fields)  ; projeção - coluna D
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
        (princ "\nDados extraídos com sucesso:")
        (princ (strcat "\nCota: " (nth 0 found-data)))
        (princ (strcat "\nProfundidade: " (rtos (nth 1 found-data))))
        (princ (strcat "\nProjeção: " (nth 2 found-data)))
        (princ (strcat "\nN.A.: " (nth 3 found-data)))
        found-data
      )
      (progn
        (princ (strcat "\nSondagem '" nome-sondagem "' não encontrada"))
        nil
      )
    )
  )
  
  ;;----------------------------------------------------------------------------
  ;; FUNÇÃO: DIVISÃO DE STRING POR DELIMITADOR
  ;;----------------------------------------------------------------------------
  ;; Divide uma string usando um delimitador específico
  ;; Parâmetros:
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
  ;; INÍCIO DA FUNÇÃO PRINCIPAL
  ;;============================================================================
  (princ "\n==============================================================================")
  (princ "\n                 GERADOR DE PALITOS DE SONDAGEM")
  (princ "\n==============================================================================")
  (princ "\nIniciando comando SONDM...")
  
  ;;----------------------------------------------------------------------------
  ;; ENTRADA DE DADOS DO USUÁRIO
  ;;----------------------------------------------------------------------------
  ;; Solicita nome da sondagem
  (setq nome (getstring "\nDigite o nome da sondagem: "))
  
  ;; Valida entrada do usuário
  (if (or (not nome) (= nome ""))
    (progn
      (princ "\nERRO: Nome da sondagem não pode ser vazio")
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
      (setq proj (nth 2 csv-data))      ; Projeção
      (setq na2 (nth 3 csv-data))       ; Nível d'água
      
      ;; Validação dos dados extraídos
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
      
      ;; Exibe dados encontrados
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
  
  ;;----------------------------------------------------------------------------
  ;; SELEÇÃO DO PONTO DE INSERÇÃO
  ;;----------------------------------------------------------------------------
  (setq p1 (getpoint "\nClique o ponto de inserção do palito: "))
  
  ;; Valida ponto selecionado
  (if (not p1)
    (progn
      (princ "\nERRO: Ponto de inserção não selecionado")
      (exit)
    )
  )
  
  ;;----------------------------------------------------------------------------
  ;; CONFIGURAÇÃO DO AMBIENTE DE DESENHO
  ;;----------------------------------------------------------------------------
  ;; Salva variáveis atuais do sistema
  (setq osn (getvar "osmode"))          ; Modo de snap
  (setq ml (getvar "clayer"))           ; Layer atual
  (setq mt (getvar "textstyle"))        ; Estilo de texto
  (setvar "osmode" 0)                   ; Desativa snap durante desenho

  ;;============================================================================
  ;; CRIAÇÃO DA BANDEIRA INFORMATIVA
  ;;============================================================================
  (princ "\nCriando bandeira informativa...")
  
  ;; Cálculo dos pontos da bandeira
  (setq p2 (polar p1 (* (/ 90.0 180.0) pi) 6.5))     ; Ligação do palito com a bandeira
  (setq p3 (polar p2 (* (/ 180.0 180.0) pi) 6.75))   ; Ponto esquerdo baixo da bandeira
  (setq p4 (polar p3 (* (/ 90.0 180.0) pi) 5.0))     ; Ponto esquerdo alto da bandeira
  (setq p5 (polar p4 (* (/ 0.0 180.0) pi) 13.5))     ; Ponto direito alto da bandeira
  (setq p6 (polar p5 (* (/ 270.0 180.0) pi) 5.0))    ; Ponto direito baixo da bandeira
  (setq p7 (polar p4 (* (/ 270.0 180.0) pi) 2.0))    ; Ponto esquerdo da 1ª divisão
  (setq p8 (polar p7 (* (/ 0.0 180.0) pi) 13.5))     ; Ponto direito da 1ª divisão
  (setq p9 (polar p7 (* (/ 270.0 180.0) pi) 1.5))    ; Ponto esquerdo da 2ª divisão
  (setq p10 (polar p9 (* (/ 0.0 180.0) pi) 13.5))    ; Ponto direito da 2ª divisão
  
  ;; Pontos para posicionamento dos textos
  (setq p11 (polar p2 (* (/ 90.0 180.0) pi) 4.0))    ; Texto de identificação
  (setq p12 (polar p2 (* (/ 90.0 180.0) pi) 2.25))   ; Texto da cota
  (setq p13 (polar p2 (* (/ 90.0 180.0) pi) 0.75))   ; Texto da projeção

  ;; Criação do layer para perfil/contorno
  (command "_layer" "m" "msp-ge_perfil" "c" "3" "msp-ge_perfil" "")
  
  ;; Desenho da estrutura da bandeira
  (command "_line" p1 p2 "")                          ; Ligação bandeira-palito
  (command "_pline" p3 p4 p5 p6 "close")              ; Contorno da bandeira
  (command "_line" p7 p8 "")                          ; Primeira divisão
  (command "_line" p9 p10 "")                         ; Segunda divisão

  ;; Criação do layer para textos
  (command "_layer" "m" "msp-ge_textos" "c" "2" "msp-ge_textos" "")
  
  ;; Inserção dos textos informativos
  (command "text" "j" "MC" p11 1.5 0 nome)                      ; Nome da sondagem
  (command "text" "j" "MC" p12 1.0 0 cota)                      ; Cota da sondagem
  (command "text" "j" "MC" p13 1.0 0 (strcat "Proj.: " proj "m")) ; Projeção

  ;;============================================================================
  ;; CRIAÇÃO DO PALITO DE SONDAGEM
  ;;============================================================================
  (princ "\nCriando palito de sondagem...")
  
  ;; Cálculo dos pontos do palito
  (setq p14 (polar p1 (* (/ 180.0 180.0) pi) 0.375))           ; Ponto esquerdo alto
  (setq p15 (polar p1 (* (/ 0.0 180.0) pi) 0.375))             ; Ponto direito alto
  (setq p16 (polar p14 (* (/ 270.0 180.0) pi) (* prof 2.5)))   ; Ponto esquerdo baixo
  (setq p17 (polar p15 (* (/ 270.0 180.0) pi) (* prof 2.5)))   ; Ponto direito baixo
  (setq p18 (polar p1 (* (/ 270.0 180.0) pi) (+ (* prof 2.5) 1.7))) ; Ponto do texto da profundidade
  
  ;; Desenho do palito
  (command "_layer" "m" "msp-ge_perfil" "c" "3" "msp-ge_perfil" "")
  (command "_pline" p14 p16 p17 p15 "close")          ; Contorno do palito
  
  ;; Inserção do texto da profundidade
  (command "_layer" "m" "msp-ge_textos" "c" "2" "msp-ge_textos" "")
  (command "text" "j" "MC" p18 1.25 0 (rtos prof))   ; Comprimento da sondagem
  
  ;; Ponto de referência para cálculos posteriores
  (setq p14a (polar p14 (* (/ 180.0 180.0) pi) 0))
 
  ;;============================================================================
  ;; CRIAÇÃO DO INDICADOR DE NÍVEL D'ÁGUA (N.A.)
  ;;============================================================================
  (princ "\nCriando indicador de nível d'água...")
  
  ;; Validação do valor do N.A.
  (if (not (numberp (atof na2)))
    (progn
      (princ "\nAVISO: Valor N.A. inválido, usando 0")
      (setq na2 "0")
    )
  )
  
  ;; Cálculo dos pontos do indicador N.A.
  (setq p19 (polar p14 (* (/ 180.0 180.0) pi) 1.5))           ; Ponto direito alto
  (setq p20 (polar p19 (* (/ 270.0 180.0) pi) (* 2.5 (atof na2)))) ; Ponto direito
  (setq p21 (polar p20 (* (/ 180.0 180.0) pi) 1.25))         ; Ponto do meio
  (setq p22 (polar p21 (* (/ 180.0 180.0) pi) 1.25))         ; Ponto esquerdo
  (setq p23 (polar p21 (* (/ 270.0 180.0) pi) 1.25))         ; Ponto abaixo
  
  ;; Inserção do texto N.A.
  (command "text" "j" "BC" p21 1 0 "N.A.")
  
  ;; Criação do layer para N.A.
  (command "_layer" "m" "msp-ge_NA" "c" "1" "msp-ge_NA" "")
  
  ;; Desenho do indicador N.A.
  (command "_pline" p20 p22 p23 "close")              ; Contorno do N.A.
  (command "_solid" p20 p22 p23 p20 "")               ; Preenchimento sólido

  ;;============================================================================
  ;; CRIAÇÃO DE MARCAÇÕES E TEXTOS NO PALITO
  ;;============================================================================
  (princ "\nCriando marcações no palito...")
  
  ;; Cálculo dos pontos iniciais para marcações
  (setq p24 (polar p14 (* (/ 270.0 180.0) pi) 2.5))   ; Ponto baixo esquerdo do 1º sólido
  (setq p25 (polar p24 (* (/ 0.0 180.0) pi) 0.75))    ; Ponto baixo direito do 1º sólido
  (setq p26 (polar p24 (* (/ 270.0 180.0) pi) 2.5))   ; Ponto alto esquerdo do sólido seguinte

  ;; Cálculo das distâncias para controle do loop
  (setq dist1 (distance p14 p16))                     ; Distância total do furo
  (setq dist2 (distance p14 p24))                     ; Distância do primeiro metro
  (setq dist3 (distance p14 p26))                     ; Distância alta do sólido seguinte

  ;; Criação das marcações ao longo do palito
  (if (<= dist1 dist2)
    ;; Se a profundidade é menor que um metro
    (progn
      (command "_layer" "m" "msp-ge_perfil" "c" "3" "msp-ge_perfil" "")
      (command "_solid" p14 p15 p16 p17 "")
      (setq p27 (polar p17 (* (/ 0.0 180.0) pi) 0.5))
      (command "_layer" "m" "msp-ge_textos" "c" "2" "msp-ge_textos" "")
      (command "text" "j" "l" p27 1 0 "-")            ; Marcação de golpes
    )
    ;; Se a profundidade é maior que um metro, criar marcações intervaladas
    (while (< dist3 dist1)
      (if (<= (+ dist3 2.5) dist1)
        (progn
          (setq p28 (polar p14 (* (/ 270.0 180.0) pi) 2.5))
          (setq p29 (polar p28 (* (/ 0.0 180.0) pi) 0.75))
          (setq p30 (polar p29 (* (/ 0.0 180.0) pi) 0.5))
          (command "_layer" "m" "msp-ge_perfil" "c" "3" "msp-ge_perfil" "")
          (command "_line" p28 p29 "")
          (command "_layer" "m" "msp-ge_textos" "c" "2" "msp-ge_textos" "")
          (command "text" "j" "l" p30 1 0 "-")        ; Marcação de golpes
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
  ;; FINALIZAÇÃO
  ;;============================================================================
  ;; Restaura variáveis do sistema
  (setvar "clayer" ml)
  (setvar "osmode" osn)
  (setvar "textstyle" mt)
  
  ;; Mensagem de conclusão
  (princ "\n==============================================================================")
  (princ "\nPalito de sondagem criado com sucesso!")
  (princ (strcat "\nSondagem: " nome))
  (princ (strcat "\nCota: " cota))
  (princ (strcat "\nProfundidade: " (rtos prof) " m"))
  (princ (strcat "\nProjeção: " proj " m"))
  (princ (strcat "\nN.A.: " na2 " m"))
  (princ "\n==============================================================================")
  (princ)
)