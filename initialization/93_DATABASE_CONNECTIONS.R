
# Connection to the Global Tuna Atlas
con_GTA = dbConnect(drv = PostgreSQL(),
               dbname   = "tunaatlas", 
               user     = "tunaatlas_inv",
               password = "fle087",
               host     = "db-tunaatlas.d4science.org")

