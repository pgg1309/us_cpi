source("rscripts/00_calculate_monthly_core.R")
source("rscripts/10_read_official_cpi.R")

library(blastula)

email <- render_email("rmds/core_cpi_monthly_report.Rmd", quiet = FALSE)

# create_smtp_creds_key(
#   id = 'jgp',
#   user = 'pgrahl@jgpglobal.com.br',
#   host = 'smtp.office365.com',
#   port = 587,
#   use_ssl = TRUE
# )



email %>%
  smtp_send(
    from = 'pgrahl@jgpglobal.com.br',
    to = 'pgrahl@jgpglobal.com.br',
    subject = paste("Us Monthly CPI Report", format(max(last.date), "%B/%Y"), sep = " - "),
    credentials = creds_key(id = "jgp")
  )

