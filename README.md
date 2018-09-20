# Búfalos

Script e dados para estimativa populacional de búfalos na REBIO Piratuba e ESEC Maracá-Jipioca, 2017 

## arquivos

**bufalos.csv** é o arquivo com dados de contagens aéreas (método de contagem dupla) realizadas na REBIO Piratuba e ESEC Maracá-Jipioca em dezembro de 2017. Dados coletados por Elildo Carvalho Jr e Henrique Santos.

**double_count_analysis_code.R** é o script para rodar as análises (estimativas populacionais, gráficos etc.).

**double_count_function.R** é o script com as funções double.count e ci.count para estimativa populacional e cálculo de intervalo de confiança (bootstrap) respectivamente. O script anterior chama essas funções para rodar as análises.
