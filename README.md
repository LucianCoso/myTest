OpenCPU APP: appdemo
====================
  
The purpose of this OpenCPU app is to demonstrate R capabilities regarding machine learning.

Cloud Demo
----------

Try the application online at the OpenCPU apps public demo server: https://luciancoso.ocpu.io/myTest/www/

Use locally
-----------

To run the app in your local R session:

    #install app
    library(devtools)
    install_github("LucianCoso/myTest")
    
    #load in opencpu
    library(opencpu)
    opencpu$browse("/library/myTest/www")
