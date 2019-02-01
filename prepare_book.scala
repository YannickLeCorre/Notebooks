/* read all the directories & files starting from the root directory provided
 as input. Then read all the R files and transform their content in order to
 remove the header part and to increase all the header tags by one (ie ## -> ###).

 Then copy all the files (Rmd + all references) into the target directory called
 BOOK in a flat structure).

  The templae directory contains the files _yml and project that are needed to
  build the notebook from R.

  The file content _bookdown.yml is modified by adding a line with a reference
  to all the Rmd files.
*/

/*
  Note: the processing results can be tested using diff
    diff -y "R language/R_language.Rmd" BOOK/R_language.Rmd  | more
*/
val BOOK_DIR_NAME: String = "BOOK" // name of the target directory that contains the files for the book
val BOOK_TEMPLATE_DIR_NAME: String = "BOOK_TEMPLATE"
val SOURCE_DIR_TEST = "/home/yannick/shared/TEST"

val EXCLUDED_DIR = List[String](BOOK_DIR_NAME, BOOK_TEMPLATE_DIR_NAME) // list of the directories that do not contain notebooks

import java.io.{ File, BufferedWriter, FileWriter }
import java.nio.file.{ Files, Paths }
import scala.util.matching.Regex
import scala.io.Source // to read the files
import scala.collection.mutable.ArrayBuffer

val imageRefPattern: Regex = """!\[([ \.\-\(\)_0-9a-zA-Z]+)\]*\(([_0-9a-zA-Z\.\-]+)\)""".r // Regex to identify the link to images within a Rmd file
val titlePattern: Regex = """w*: \"([_A-Za-z ]+)\"""".r // Regex retrieve the title of the Rmd (first line)

/*
  remove a non empty directory. Cannot use FileUtils.detele when a directory is not empty. It is assumed thet the directory is flat (ie it does not contain sub-directories)
*/
def removeDir(dir: File, verbose: Boolean = true): Unit = {
  if (dir.isDirectory) {
    if (verbose) println(s"removing directory: ${dir.getAbsolutePath}...")
    for (file <- dir.listFiles) removeDir(file, verbose)
    dir.delete
  } else { dir.delete; if (verbose) println(s"file: ${dir.getAbsolutePath} deleted") }
}

/*
  Copy all the files from the templateDir into the targetDir and create an empty DATA folder
*/
def initializeTargetBook(templateDir: File, targetDir: File, verbose: Boolean = true): Unit = {
  if (verbose) println(s"Initializing the BOOK directory...")
  if (targetDir.exists) removeDir(targetDir)
  targetDir.mkdir()
  if (verbose) println(s"${targetDir.getAbsolutePath} created")
  if ((templateDir.isDirectory) && (targetDir.isDirectory)) {
    for (file <- templateDir.listFiles) {
      val p1 = Paths.get(file.getAbsolutePath)
      val p2 = Paths.get(targetDir.getAbsolutePath + "/" + file.getName)
      if (verbose) println(s"copying ${p1} into ${p2}")
      Files.copy(p1, p2)
    }
    val dataDir: File = new File(targetDir.getAbsolutePath + "/DATA")
    dataDir.mkdir()
    //Files.createDirectory(targetDir.toPath)
  }
}

//def transformAndMoveIntoNotebookFolder(inputDir: String, templateDirName: String = BOOK_TEMPLATE_DIR_NAME): Unit = {
val inputDir = SOURCE_DIR_TEST
val templateDirName = BOOK_TEMPLATE_DIR_NAME
println("input directory: " + inputDir)
val outputDir: String = inputDir + "/" + BOOK_DIR_NAME
println("output directory: " + outputDir)
val inputD = new File(inputDir)
val templateDir: String = inputDir + "/" + templateDirName
println("template directory: " + templateDir)
if (!inputD.exists || !inputD.isDirectory) {
  println("the source directory specified (" + inputDir + ") does not exist or is not a directory")
  //return
  System.exit(1)
}
// wipe out the target directory
val outputD = new File(outputDir)
//if (outputD.exists) FileUtils.deleteDirectory(outputD)
initializeTargetBook(new File(inputDir + "/" + BOOK_TEMPLATE_DIR_NAME), outputD)
// get the list of all the directories which contain the notebooks
val dirList = inputD.listFiles.filter(_.isDirectory)
// the following two lines perform the same thing
//val EVAL_DIR = for(name <- dirList if !EXCLUDED_DIR.contains(name.getName)) yield name
val EVAL_DIR = dirList
  .filter((f: File) => !EXCLUDED_DIR.contains(f.getName))
//println("dirList: " + dirList)
//println("EVAL_DIR: " + EVAL_DIR)

/*
  This function takes reads a Rmd file and returns a Tuple:
  - a String with the content transformed by suppressing the header and leveling up all the getListOfFiles
  - a List of String which contains the list of all the images referenced by the Rmd file
*/
def evalRmd(f: File, verbose: Boolean = true): (String, List[String]) = {
  val fileName = f.getAbsolutePath
  val dirName = f.getParent
  if (verbose) println(s"analysis of the notebook: ${fileName}")

  def getTitle(line: String): String = {
    //val title = titlePattern.findFirstIn(line).getOrElse("NO_TITLE")
    val titleMatch = titlePattern.findFirstMatchIn(line)
    val title = (for (v <- titleMatch) yield v.group(1)).getOrElse("NO TITLE")
    if (verbose) println(s" title is ${title}")
    title
  }

  def getImageLink(line: String, verbose: Boolean = true): List[String] = {
    var imageList: List[String] = List()
    for (patternMatch <- imageRefPattern.findAllMatchIn(line)) {
      if (verbose) println(s"text: ${patternMatch.group(1)}, filename: ${patternMatch.group(2)}")
      //      imageList +:= dirName + "/" + patternMatch.group(2)
      imageList +:= patternMatch.group(2)
    }
    imageList
  }

  var i = 0
  var endHeader = false
  //var newText = new Array[String](0)
  val newText: ArrayBuffer[String] = ArrayBuffer()
  var ii = -1
  var title: String = ""
  var imageFileNames: List[String] = List()
  for (line <- Source.fromFile(fileName).getLines) {
    i = i + 1
    if (i == 2) title = getTitle(line)
    else if (!endHeader) {
      if ((i > 1) && (line == "---")) endHeader = true
    } else {
      ii = ii + 1
      //      if (ii == 0) newText +:= "#" + title
      if (ii == 0) newText += "#" + title
      else {
        imageFileNames = imageFileNames ::: getImageLink(line)
        newText += {
          if ((line.length > 0) && (line(0) == '#')) "#".concat(line) // all the titles are leveled up (eg ##title -> ###title)
          else line.toString
        }
      }
    }
  }

  //println(s"file has ${i} lines; newText has ${ii} lines")
  //println(newText.toList)
  //println(s"imageFileNames: ${imageFileNames.toList}")
  //return List(newText, imageFileNames)
  return (newText.mkString("\r\n"), imageFileNames)
}

def copyFilesToBook(file: File, content: String, images: List[String], verbose: Boolean = true): Unit = {
  val fileName = file.getName
  val dirName = file.getParent
  if (verbose) println(s"fileName: ${fileName}; dirName: ${dirName}")
  // writinh the formated notebook
  val copyFile = new File(outputDir + "/" + fileName)
  val bw = new BufferedWriter(new FileWriter(copyFile))
  bw.write(content)
  bw.close()
  // copying the imageList
  for (imageName <- images) {
    val p1 = Paths.get(dirName + "/" + imageName)
    val p2 = Paths.get(outputDir + "/" + imageName)
    if (verbose) println(s"p1: ${p1}; p2: ${p2}")
    Files.copy(p1, p2)
  }
  // copying all the files in the DATA directory (if any)
  val dataDir = new File(dirName + "/DATA")
  if (dataDir.exists) {
    for (file <- dataDir.listFiles) {
      val targetDataDir = Paths.get(outputDir + "/DATA/" + file.getName)
      if (verbose) println(s"copying DATA file ${file.toPath} to ${targetDataDir}")
      Files.copy(file.toPath, targetDataDir)
    }
  }
}

val notebookNames: ArrayBuffer[String] = ArrayBuffer("index.Rmd") // store the name of all the notebooks to assemble in the book
for (dir <- EVAL_DIR) {
  val evalList = dir.listFiles.toList
    .filter((f: File) => f.getName.contains(".Rmd"))
  for (file <- evalList) {
    val (text, images) = evalRmd(file)
    copyFilesToBook(file, text, images)
    notebookNames += file.getName
  }
}

/*
 adding the line in the file _bookdown_yml
 rmd_files: ["index.Rmd", "R_language.Rmd", "Resume_Analysis.Rmd"]
*/
val line = "rmd_files: [" + notebookNames.map(e => '"' + e + '"').mkString(",") + "]\r\n"
println(s"line: ${line}")
val fw = new FileWriter(outputDir + "/_bookdown.yml", true)
fw.write(line)
fw.close()

/*
// snippet qui test le regex pour les déterminer les hyperliens de type images
  import scala.util.matching.Regex
  val imageRefPattern: Regex = """!\[([ 0-9a-zA-Z]+)\]*\(([0-9a-zA-Z\.]+)\)""".r
  val input: String =
  """Voici une texte bannal ![1er texte](fichier1.png) qui est ici
    |![2eme texte](fichier2.jpeg) mais encore ![3eme texte](fichier3.jpeg) pour finir
  """.stripMargin
  println(input)
  for (patternMatch <- imageRefPattern.findAllMatchIn(input)) {
    println(patternMatch)
    println(s"text: ${patternMatch.group(1)}, filename: ${patternMatch.group(2)}")
    }
    */
//println(evalList)
//}

//transformAndMoveIntoNotebookFolder(SOURCE_DIR_TEST)

System.exit(0)

/*
def getListOfFiles(dir: String):List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
        d.listFiles.filter(_.isFile).toList
    } else {
        List[File]()
    }
}

//The REPL demonstrates how you can use this method:


val files = getListOfFiles("..")
//iles: List[java.io.File] = List(/tmp/foo.log, /tmp/Files.scala.swp)
for (f <- files) println(f)
*/

/* snippet to copy a file1 into file2
import java.nio.file.{Files, Paths}

val p1 = Paths.get(SOURCE_DIR_TEST + "/test.scala")
val p2 = Paths.get(SOURCE_DIR_TEST + "/toto")
Files.copy(p1, p2)

*/
