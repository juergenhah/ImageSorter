The module copies all files ending with "jpg" or "JPG" from an directory into subdirectories.
Subdirectories are created according the dates included into the Exif part 
of the images.

>module ImageSorter (printImageDates,createDateDirectories,sortImageDirectory, Directory ) where

>import Graphics.HsExif as ExifLib
>import Data.Time.Calendar
>import Data.Time.LocalTime
>import System.Directory
>import Data.List

>type Directory = FilePath
>type Image = FilePath

path of the image directory 

>workingImageDirectory = "/home/juergen/Pictures/California2014/400"

Parses an image and returns the date stored in Exif part of the image.

>getExifDateTimeFromImage :: Image -> IO (Maybe LocalTime)
>getExifDateTimeFromImage image = do
>  img <- ExifLib.parseFileExif image
>  case img of
>   Right exifData -> return $ getDateTimeOriginal exifData 
>   Left msg -> return Nothing


>exifDateTimeTest = do
>  maybeTime <- getExifDateTimeFromImage (workingImageDirectory ++ "/" ++ "2014-07-06 09.01.38.jpg")
>  case maybeTime of
>    Nothing -> putStrLn "no time included in Exif part of image" 
>    Just dateTime ->  putStrLn $ show $ dateTime  


Prints the date included into the Exif part of an image
output format: YYYY-MM-DD; example: 2014-07-06

>printExifDate ::Directory -> Image -> IO ()
>printExifDate imageDirectory image = do
>  maybeDateTime <- getExifDateTimeFromImage (imageDirectory++"/"++image)
>  case maybeDateTime of
>   Nothing -> putStrLn ("no time included in Exif part of image:" ++ image)
>   Just dateTime -> putStrLn $ showGregorian.localDay $ dateTime 

Prints dates for all images ending with "JPG" or "jpg" 
included into the imageDirectory

>printImageDates:: Directory -> IO()
>printImageDates imageDirectory = do
>  allFiles <- getDirectoryContents imageDirectory
>  let jpegImages = filter (\i -> "JPG" `isSuffixOf` i || "jpg" `isSuffixOf` i) allFiles
>  mapM_ (printExifDate imageDirectory) jpegImages
>  putStrLn "finished"

>printTest = printImageDates workingImageDirectory

Reads the Exif date of the image and creates a subfolder with this date.

>createDateDirectory ::Directory -> Image -> IO ()
>createDateDirectory imageDirectory image = do
>  maybeDateTime <- getExifDateTimeFromImage (imageDirectory++"/"++image)
>  case maybeDateTime of
>   Nothing -> putStrLn ("no time included in Exif part of image:" ++ image)
>   Just dateTime -> do
>        let date = (showGregorian.localDay) dateTime
>        let newDirectoryPath = imageDirectory ++ "/" ++ date
>        test<- doesDirectoryExist newDirectoryPath
>        if test then
>          return ()-- "directory exists already"
>        else
>          createDirectory newDirectoryPath

Creates subfolders according to the image dates of the included images in this folder

>createDateDirectories :: Directory -> IO ()
>createDateDirectories imageDirectory = do
>  allFiles <- getDirectoryContents imageDirectory
>  let jpegImages = filter (\i -> "JPG" `isSuffixOf` i || "jpg" `isSuffixOf` i) allFiles
>  mapM_ (createDateDirectory imageDirectory) jpegImages
>  putStrLn "directory creating finished"

>createDirectoryTest = createDateDirectories workingImageDirectory


Copies the image into an folder that is named with the date found in the 
Exif part. The folder must exist, after copying the old image is deleted.
The image is renamed with the date and time according the Exif date time.
Pattern for remaning: YYYY-MM-DD HH:MM:SS

>imageCopyAndDelete ::Directory -> Image -> IO () 
>imageCopyAndDelete imageDirectory image = do
>  maybeDateTime <- getExifDateTimeFromImage (imageDirectory++"/"++image)
>  case maybeDateTime of
>   Nothing -> putStrLn ("no time included in Exif part of image:" ++ image)
>   Just dateTime -> do 
>       copyFile imagePath (imageDirectory++"/" ++ (showGregorian $ localDay dateTime)++"/"++ (show dateTime)++".jpg" ) 
>       removeFile imagePath   
>  where imagePath = imageDirectory ++ "/"++ image 

Copies "JPG" and "jpg" images of the directory in EXISTING subdirectories according
the date included into the Exif part and removes the image in the directory.

>copyAndDeleteImages :: Directory -> IO () 
>copyAndDeleteImages imageDirectory = do
>  allFiles <- getDirectoryContents imageDirectory
>  let jpegImages = filter (\i -> "JPG" `isSuffixOf` i || "jpg" `isSuffixOf` i) allFiles
>  contents <-mapM (imageCopyAndDelete imageDirectory) jpegImages
>  putStrLn "copying and deleting completed"

>copyADITest= copyAndDeleteImages workingImageDirectory

Creates subdirectories depending on the dates of the images and copies
the images in the according subdirectory.

>sortImageDirectory :: Directory -> IO ()
>sortImageDirectory imageDirectory= do
>  createDateDirectories imageDirectory
>  copyAndDeleteImages imageDirectory
>  putStrLn "process completed"

>sortTest=sortImageDirectory workingImageDirectory
