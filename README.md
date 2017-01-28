# Image-processing / machine learning - Removed coffee stains (noise) from dirty documents
 
Developed multiple image processing and machine learning approaches in R to remove noise in the scanned documents and ensemble using Xgboost to improve the output quality of the OCR process of digitizing documents.

Tools - R
Techniques - 
  1. Edge detection and morphology (BiOps package)
  2. Median filtering
  3. Adaptive thresholding
  4. Feature engineering - Identifying gaps between sentences
  5. XgBoost ensemble

"ImageCleaning.R" - R code - impelemted algorithms for image processing.
"XGboost.R" - Complements "ImageCleaning.R" ensembling different techniques.
"TeamsmaRt_Project1_Image_Processing.pptx" - White-Paper for the project.
"TeamsmaRt_Project1_Image_Processing.pptx" - Powerpoint Presentation of project.
"XGBoost_Workspace_Image.RData" - RData file with preinitialized variables (to save time to load the images in vectors)
"train_sample" - Sample images folder.
