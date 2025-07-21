;==============================================================================
; survey-info
;==============================================================================
; Criação de palitos de sondagem no AutoCAD com dados extraídos de CSV
; Autor: andrebmuller
; Versão: 1.6.0
;==============================================================================
; ESTRUTURA DO CSV ESPERADA:
; Coluna A: ID Sondagem      | Coluna E: NA (Nível d'água)
; Coluna B: Cota de boca     | Coluna F: Profundidade inicial  
; Coluna C: Prof. máxima     | Coluna G: Profundidade final
; Coluna D: Projeção         | Coluna H: SPT (valores ou "-")
;==============================================================================

(defun c:SONDM (/ 
                   ;; VARIÁVEIS DE DADOS
                   nome csv-data cota prof proj na2 valores-coluna8 
                   csv-path dwg-dir contador golpes
                   
                   ;; VARIÁVEIS DE PONTOS GEOMÉTRICOS
                   p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 
                   p14 p15 p16 p17 p18 p14a p19 p20 p21 p22 p23 
                   p24 p25 p26 p27 p28 p29 p30 p30a p14b p15b p17a
                   
                   ;; VARIÁVEIS DE CONTROLE E CÁLCULO
                   dist1 dist2 dist3 spt-count altura-real-palito altura-final
                   p14_final p15_final p16_final p17_final p16_frame p17_frame 
                   prof-spt p16_last p17_last
                   
                   ;; VARIÁVEIS DE AMBIENTE AUTOCAD
                   osn ml mt ang90 ang180 ang270 ang0)

  ;;============================================================================
  ;; INICIALIZAÇÃO E CONFIGURAÇÕES
  ;;============================================================================
  
  ;; Pré-cálculo de ângulos para otimização de performance
  (setq ang0    0.0          ; 0 graus
        ang90   (/ pi 2)     ; 90 graus  
        ang180  pi           ; 180 graus
        ang270  (* 3 (/ pi 2)) ; 270 graus
        contador 0)          ; Contador para loop SPT

  ;;============================================================================
  ;; FUNÇÕES UTILITÁRIAS
  ;;============================================================================
  
  ;;----------------------------------------------------------------------------
  ;; FUNÇÃO: LEITURA E PROCESSAMENTO DE ARQUIVO CSV
  ;; Parâmetros: csv-path - Caminho completo do arquivo CSV
  ;; Retorna: Lista com [dados-sondagem, valores-spt] ou nil se erro
  ;;----------------------------------------------------------------------------
  (defun read-single-survey-csv (csv-path / csv-file line-data fields survey-data valores-h)
    ;; Verifica se arquivo existe
    (if (not (findfile csv-path))
      (return nil)
    )
    
    ;; Tenta abrir arquivo para leitura
    (setq csv-file (open csv-path "r"))
    (if (not csv-file)
      (return nil)
    )
    
    ;; Ignora linha de cabeçalho
    (read-line csv-file)
    
    ;; Lê primeira linha de dados (contém informações principais da sondagem)
    (setq line-data (read-line csv-file))
    (if (not line-data)
      (progn
        (close csv-file)
        (return nil)
      )
    )
    
    ;; Processa primeira linha e valida estrutura
    (setq fields (split-string line-data ";"))
    (if (< (length fields) 8)
      (progn
        (close csv-file)
        (return nil)
      )
    )
    
    ;; Extrai dados principais da sondagem (colunas A-E)
    (setq survey-data (list
      (nth 0 fields)                      ; A: Nome da sondagem
      (nth 1 fields)                      ; B: Cota de boca
      (atof (nth 2 fields))               ; C: Profundidade máxima
      (nth 3 fields)                      ; D: Projeção
      (nth 4 fields)                      ; E: Nível d'água (N.A.)
    ))
    
    ;; Inicializa coleta de valores SPT (coluna H)
    (setq valores-h '())
    
    ;; Adiciona valor SPT da primeira linha se existir
    (if (and (nth 7 fields) 
             (> (strlen (nth 7 fields)) 0))
      (setq valores-h (cons (nth 7 fields) valores-h))
    )
    
    ;; Processa linhas subsequentes para coletar todos os valores SPT
    (while (setq line-data (read-line csv-file))
      (if line-data
        (progn
          (setq fields (split-string line-data ";"))
          ;; Verifica se linha tem estrutura adequada e valor SPT válido
          (if (>= (length fields) 8)
            (progn
              ;; Coleta valor SPT (coluna H, índice 7) incluindo "-"
              (if (and (nth 7 fields) 
                       (> (strlen (nth 7 fields)) 0))
                (setq valores-h (cons (nth 7 fields) valores-h))
              )
            )
          )
        )
      )
    )
    
    ;; Fecha arquivo e limpa variável
    (close csv-file)
    
    ;; Retorna dados estruturados: [informações-sondagem, valores-spt-ordenados]
    (list survey-data (reverse valores-h))
  )
  
  ;;----------------------------------------------------------------------------
  ;; FUNÇÃO: DIVISÃO DE STRING POR DELIMITADOR
  ;; Parâmetros: str - String a ser dividida, delimiter - Caractere separador
  ;; Retorna: Lista com as partes da string
  ;;----------------------------------------------------------------------------
  (defun split-string (str delimiter / result pos)
    (setq result '())
    ;; Procura delimitador e divide string iterativamente
    (while (setq pos (vl-string-search delimiter str))
      (setq result (cons (substr str 1 pos) result))
      (setq str (substr str (+ pos 2)))
    )
    ;; Inverte resultado para manter ordem original
    (reverse (cons str result))
  )

  ;;============================================================================
  ;; PROGRAMA PRINCIPAL
  ;;============================================================================
  
  ;;----------------------------------------------------------------------------
  ;; SELEÇÃO E VALIDAÇÃO DE ARQUIVO
  ;;----------------------------------------------------------------------------
  
  ;; Obtém diretório do desenho atual como ponto de partida
  (setq dwg-dir (getvar "DWGPREFIX"))
  (if (or (not dwg-dir) (= dwg-dir ""))
    (setq dwg-dir "")
  )
  
  ;; Interface de seleção de arquivo CSV
  (setq csv-path (getfiled "Selecione o arquivo CSV da sondagem" dwg-dir "csv" 0))
  (if (not csv-path)
    (progn
      (princ "\nOperação cancelada pelo usuário.")
      (exit)
    )
  )
  
  ;;----------------------------------------------------------------------------
  ;; LEITURA E PROCESSAMENTO DE DADOS
  ;;----------------------------------------------------------------------------
  
  ;; Processa arquivo CSV e extrai dados
  (setq csv-data (read-single-survey-csv csv-path))
  (if (not csv-data)
    (progn
      (princ "\nERRO: Não foi possível ler o arquivo CSV ou estrutura inválida.")
      (princ "\nVerifique se o arquivo possui pelo menos 8 colunas separadas por ';'")
      (exit)
    )
  )
  
  ;; Extrai dados estruturados para variáveis de trabalho
  (setq nome           (nth 0 (nth 0 csv-data))  ; Nome da sondagem
        cota           (nth 1 (nth 0 csv-data))  ; Cota de boca
        prof           (nth 2 (nth 0 csv-data))  ; Profundidade máxima
        proj           (nth 3 (nth 0 csv-data))  ; Projeção
        na2            (nth 4 (nth 0 csv-data))  ; Nível d'água
        valores-coluna8 (nth 1 csv-data))        ; Lista de valores SPT
  
  ;; Validação de dados críticos
  (if (or (not (numberp prof)) (<= prof 0))
    (progn
      (princ "\nERRO: Profundidade máxima inválida.")
      (princ "\nVerifique se a coluna C contém um número maior que zero.")
      (exit)
    )
  )
  
  ;; Feedback para usuário
  (princ (strcat "\n▶ Sondagem: " nome))
  (princ (strcat "\n▶ SPT encontrados: " (itoa (length valores-coluna8)) " valores"))
  
  ;;----------------------------------------------------------------------------
  ;; CONFIGURAÇÃO DO AMBIENTE DE DESENHO
  ;;----------------------------------------------------------------------------
  
  ;; Solicita ponto de inserção ao usuário
  (setq p1 (getpoint "\n▶ Clique no ponto de inserção do palito: "))
  (if (not p1)
    (progn
      (princ "\nOperação cancelada.")
      (exit)
    )
  )
  
  ;; Salva configurações atuais do AutoCAD
  (setq osn (getvar "osmode")      ; Object snap
        ml  (getvar "clayer")      ; Layer atual
        mt  (getvar "textstyle"))  ; Estilo de texto
  
  ;; Configura ambiente para desenho otimizado
  (setvar "osmode" 0)  ; Desabilita object snap temporariamente

  ;;============================================================================
  ;; CRIAÇÃO DA GEOMETRIA
  ;;============================================================================

  ;;----------------------------------------------------------------------------
  ;; CRIAÇÃO DA BANDEIRA INFORMATIVA
  ;;----------------------------------------------------------------------------
  
  (princ "\n▶ Criando bandeira...")
  
  ;; Cálculo otimizado dos pontos da bandeira
  (setq p2  (polar p1 ang90 6.5)      ; Topo da haste
        p3  (polar p2 ang180 6.75)    ; Canto da bandeira
        p4  (polar p3 ang90 5.0)      ; Topo da bandeira
        p5  (polar p4 ang0 13.5)      ; Canto superior direito
        p6  (polar p5 ang270 5.0)     ; Canto inferior direito
        p7  (polar p4 ang270 2.0)     ; Primeira linha horizontal
        p8  (polar p7 ang0 13.5)      ; Fim da primeira linha
        p9  (polar p7 ang270 1.5)     ; Segunda linha horizontal
        p10 (polar p9 ang0 13.5)      ; Fim da segunda linha
        p11 (polar p2 ang90 4.0)      ; Posição texto nome
        p12 (polar p2 ang90 2.25)     ; Posição texto cota
        p13 (polar p2 ang90 0.75))    ; Posição texto projeção

  ;; Criação da geometria da bandeira
  (command "_layer" "m" "msp-ge_perfil" "c" "3" "msp-ge_perfil" "")
  (command "_line" p1 p2 "")                      ; Haste vertical
  (command "_pline" p3 p4 p5 p6 "close")          ; Retângulo da bandeira
  (command "_line" p7 p8 "")                      ; Primeira linha divisória
  (command "_line" p9 p10 "")                     ; Segunda linha divisória

  ;; Inserção dos textos informativos
  (command "_layer" "m" "msp-ge_textos" "c" "2" "msp-ge_textos" "")
  (command "text" "j" "MC" p11 1.5 0 nome)                        ; Nome da sondagem
  (command "text" "j" "MC" p12 1.0 0 cota)                        ; Cota de boca
  (command "text" "j" "MC" p13 1.0 0 (strcat "Proj.: " proj "m")) ; Projeção

  ;;----------------------------------------------------------------------------
  ;; PREPARAÇÃO DO PALITO (SEM FRAME INICIAL)
  ;;----------------------------------------------------------------------------
  
  ;; Define pontos base do palito
  (setq p14  (polar p1 ang180 0.375)  ; Lado esquerdo do palito
        p15  (polar p1 ang0 0.375)    ; Lado direito do palito
        p14a p14)                     ; Referência para cálculos posteriores
  
  ;; Inicializa variáveis para rastreamento de posição final
  (setq p14_final p14
        p15_final p15)
 
  ;;----------------------------------------------------------------------------
  ;; CRIAÇÃO DO INDICADOR DE NÍVEL D'ÁGUA (N.A.)
  ;;----------------------------------------------------------------------------
  
  (princ "\n▶ Criando indicador N.A...")
  
  ;; Cálculo das posições do indicador triangular
  (setq p19 (polar p14 ang180 1.5)                        ; Base do indicador
        p20 (polar p19 ang270 (* 2.5 (atof na2)))         ; Posição do triângulo
        p21 (polar p20 ang180 1.25)                       ; Vértice do triângulo
        p22 (polar p21 ang180 1.25)                       ; Base esquerda
        p23 (polar p21 ang270 1.25))                      ; Base inferior
  
  ;; Criação do indicador N.A.
  (command "text" "j" "BC" p21 1 0 "N.A.")               ; Texto identificador
  (command "_layer" "m" "msp-ge_NA" "c" "1" "msp-ge_NA" "")
  (command "_pline" p20 p22 p23 "close")                  ; Contorno do triângulo
  (command "_solid" p20 p22 p23 p20 "")                   ; Preenchimento sólido

  ;;----------------------------------------------------------------------------
  ;; CRIAÇÃO DAS MARCAÇÕES SPT
  ;;----------------------------------------------------------------------------
  
  (princ "\n▶ Criando marcações SPT...")
  
  ;; Pontos de referência para controle de posicionamento
  (setq p24 (polar p14 ang270 2.5)    ; Primeira marca de profundidade
        p26 (polar p24 ang270 2.5))   ; Segunda marca de profundidade
  
  ;; Verifica se existem valores SPT para processar
  (if (> (length valores-coluna8) 0)
    (progn
      ;; Calcula altura total baseada no número de valores SPT
      (setq prof-spt (* (length valores-coluna8) 2.5))
      
      (if (<= prof-spt 2.5)
        ;;------------------------------------------------------------------
        ;; CASO ESPECIAL: Profundidade menor que um metro
        ;;------------------------------------------------------------------
        (progn
          (setq p16_final (polar p14 ang270 prof-spt)
                p17_final (polar p15 ang270 prof-spt))
          
          ;; Cria solid único para profundidade pequena
          (command "_layer" "m" "msp-ge_perfil" "c" "3" "msp-ge_perfil" "")
          (command "_solid" p14 p15 p16_final p17_final "")
          
          ;; Adiciona texto SPT
          (setq p27 (polar p17_final ang0 0.5))
          (command "_layer" "m" "msp-ge_textos" "c" "2" "msp-ge_textos" "")
          (command "text" "j" "l" p27 1 0 (nth 0 valores-coluna8))
          
          ;; Atualiza posições finais
          (setq p14_final p14
                p15_final p15)
        )
        ;;------------------------------------------------------------------
        ;; CASO NORMAL: Loop para processar múltiplos valores SPT
        ;;------------------------------------------------------------------
        (while (< contador (length valores-coluna8))
          ;; Obtém valor SPT atual
          (setq golpes (nth contador valores-coluna8))
          
          ;; Calcula pontos para marcação atual
          (setq p28 (polar p14 ang270 2.5)     ; Base da linha horizontal
                p29 (polar p28 ang0 0.75)      ; Fim da linha horizontal
                p30 (polar p29 ang0 0.5))      ; Posição do texto
          
          ;; Cria linha horizontal e texto SPT
          (command "_layer" "m" "msp-ge_perfil" "c" "3" "msp-ge_perfil" "")
          (command "_line" p28 p29 "")
          (command "_layer" "m" "msp-ge_textos" "c" "2" "msp-ge_textos" "")
          (command "text" "j" "l" p30 1 0 golpes)
          
          ;; Prepara para próxima iteração
          (setq p14b (polar p28 ang270 2.5))
          (setq contador (+ contador 1))
          
          ;; Verifica se é o último valor SPT
          (if (>= contador (length valores-coluna8))
            ;;--------------------------------------------------------------
            ;; ÚLTIMO VALOR SPT: Finaliza com solid
            ;;--------------------------------------------------------------
            (progn
              (setq p15b (polar p14b ang0 0.75))
              
              ;; Calcula pontos finais corretos para o último solid
              (setq p16_last (polar p14 ang270 2.5)
                    p17_last (polar p15 ang270 2.5))
              
              ;; Cria solid final
              (command "_layer" "m" "msp-ge_perfil" "c" "3" "msp-ge_perfil" "")
              (command "_solid" p14 p15 p16_last p17_last "")
              
              ;; Atualiza posições finais para cálculo do frame
              (setq p14_final p16_last
                    p15_final p17_last)
            )
            ;;--------------------------------------------------------------
            ;; VALORES INTERMEDIÁRIOS: Continua processamento
            ;;--------------------------------------------------------------
            (progn
              (setq p15b (polar p14b ang0 0.75))
              
              ;; Cria solid intermediário e linha de continuação
              (command "_layer" "m" "msp-ge_perfil" "c" "3" "msp-ge_perfil" "")
              (command "_solid" p28 p29 p14 p15 "")
              (command "_line" p14b p15b "")
              
              ;; Atualiza pontos de referência para próxima iteração
              (setq p14 p14b
                    p15 p15b)
            )
          )
        )
      )
    )
    ;;------------------------------------------------------------------------
    ;; CASO SEM VALORES SPT: Cria marcação padrão
    ;;------------------------------------------------------------------------
    (progn
      (setq altura-final (* prof 2.5))
      (setq p16_final (polar p14 ang270 altura-final)
            p17_final (polar p15 ang270 altura-final))
      
      ;; Cria solid padrão
      (command "_layer" "m" "msp-ge_perfil" "c" "3" "msp-ge_perfil" "")
      (command "_solid" p14 p15 p16_final p17_final "")
      
      ;; Adiciona texto padrão
      (setq p27 (polar p17_final ang0 0.5))
      (command "_layer" "m" "msp-ge_textos" "c" "2" "msp-ge_textos" "")
      (command "text" "j" "l" p27 1 0 "-")
      
      ;; Define posições finais
      (setq p14_final p14
            p15_final p15)
    )
  )

  ;;----------------------------------------------------------------------------
  ;; CRIAÇÃO DO FRAME DO PALITO (APÓS MARCAÇÕES SPT)
  ;;----------------------------------------------------------------------------
  
  (princ "\n▶ Criando frame do palito...")
  
  ;; Calcula pontos do frame baseado nas posições reais finais
  (setq p16_frame (polar p14a ang270 (distance p14a p14_final))
        p17_frame (polar (polar p1 ang0 0.375) ang270 (distance p14a p14_final))
        p18       (polar p1 ang270 (+ (distance p14a p14_final) 1.7)))
  
  ;; Cria polyline do frame
  (command "_layer" "m" "msp-ge_perfil" "c" "3" "msp-ge_perfil" "")
  (command "_pline" p14a p16_frame p17_frame (polar p1 ang0 0.375) "close")
  
  ;; Adiciona texto de profundidade
  (command "_layer" "m" "msp-ge_textos" "c" "2" "msp-ge_textos" "")
  (command "text" "j" "MC" p18 1.25 0 (rtos prof))
  
  ;;============================================================================
  ;; FINALIZAÇÃO E LIMPEZA
  ;;============================================================================
  
  ;; Restaura configurações originais do AutoCAD
  (setvar "clayer" ml)     ; Layer original
  (setvar "osmode" osn)    ; Object snap original
  (setvar "textstyle" mt)  ; Estilo de texto original
  
  ;; Feedback de conclusão para o usuário
  (princ (strcat "\n✓ Palito da sondagem '" nome "' criado com sucesso!"))
  (princ (strcat "\n✓ Total de " (itoa (length valores-coluna8)) " marcações SPT processadas."))
  (princ "\n✓ Ambiente AutoCAD restaurado.")
  (princ)
)