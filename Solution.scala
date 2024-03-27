import util.Pixel
import util.Util


// Online viewer: https://0xc0de.fr/webppm/
object Solution {
  type Image = List[List[Pixel]]
  type GrayscaleImage = List[List[Double]]

  // prerequisites
  def fromStringPPM(image: List[Char]): Image = {
    val imageLines = image.mkString.split('\n')
    val imageSizeLine = imageLines.slice(1, 2)
    val imageSizes = imageSizeLine(0).split(' ')
    val imageWidth = imageSizes(0).toInt
    val imageHeight = imageSizes(1).toInt
    val imagePPM = imageLines.drop(3).map {
        imageWithoutFirst3 => imageWithoutFirst3.split(' ').map(_.toInt)
      }.grouped(imageWidth).take(imageHeight).map {
          case row => row.toList.map {
              case Array(red: Int, green: Int, blue: Int) => Pixel(red, green, blue)
            }
        }.toList
    imagePPM
  }

  def toStringPPM(image: Image): List[Char] = {
    val imageHeight = image.size
    val imageWidth = image.head.size
    val firstLine = "P3"
    val secondLine = imageWidth.toString ++ " " ++ imageHeight.toString
    val thirdLine = "255"
    val listImageFirst3Lines = (firstLine ++ "\n" ++ secondLine ++ "\n" ++ thirdLine ++ "\n").toList
    val listImagePixelLines = image.flatMap(row =>
      row.map(pixel => {
        val red = pixel.red
        val green = pixel.green
        val blue = pixel.blue
        red.toString ++ " " ++ green.toString ++ " " ++ blue.toString ++ "\n"
      })
    ).mkString.toList
    val stringPPM = listImageFirst3Lines ++ listImagePixelLines
    stringPPM
  }

  // ex 1
  def verticalConcat(image1: Image, image2: Image): Image = {
    val imageConcat = image1 ++ image2
    imageConcat
  }

  // ex 2
  def horizontalConcat(image1: Image, image2: Image): Image = {
    val imageConcat = image1.zipAll(image2, List.empty[Pixel], List.empty[Pixel]).flatMap {
      case (row1, row2) => (row1 ++ row2)
        .grouped(image1.head.length + image2.head.length).toList
    }
    imageConcat
  }

  // ex 3
  def rotate(image: Image, degrees: Integer): Image = {
    val rightDegrees = degrees % 360
    rightDegrees match {
      case 0 => image
      case 90 => image.transpose.reverse
      case _ => rotate(image.transpose.reverse, degrees - 90)
    }
  }

  // ex 4
  val gaussianBlurKernel: GrayscaleImage = List[List[Double]](
    List( 1, 4, 7, 4, 1),
    List( 4,16,26,16, 4),
    List( 7,26,41,26, 7),
    List( 4,16,26,16, 4),
    List( 1, 4, 7, 4, 1)
  ).map(_.map(_ / 273))

  val Gx : GrayscaleImage = List(
    List(-1, 0, 1),
    List(-2, 0, 2),
    List(-1, 0, 1)
  )

  val Gy : GrayscaleImage = List(
    List( 1, 2, 1),
    List( 0, 0, 0),
    List(-1,-2,-1)
  )

  def edgeDetection(image: Image, threshold : Double): Image = {
    val imageHeight = image.size
    val imageWidth = image.head.size
    val listImagePixelLines = image.flatMap(row =>
      row.map(pixel => Util.toGrayScale(pixel))
    ).grouped(imageWidth).map(_.toList).take(imageHeight).toList
    val gaussianListImagePixelLines = applyConvolution(listImagePixelLines, gaussianBlurKernel)
    val Mx = applyConvolution(gaussianListImagePixelLines, Gx)
    val My = applyConvolution(gaussianListImagePixelLines, Gy)
    val module = Mx.zipAll(My, List.empty[Double], List.empty[Double]).map {
      case (lx, ly) => lx.zipAll(ly, 0.0, 0.0).map {
        case (x, y) => x.abs + y.abs
      }
    }
    val edgeImage = module.map {
      l => l.map {
        elem =>
          if (elem < threshold) Pixel(0, 0, 0)
          else Pixel(255, 255, 255)
      }
    }
    edgeImage
  }

  def applyConvolution(image: GrayscaleImage, kernel: GrayscaleImage) : GrayscaleImage = {
    val radius = kernel.size / 2
    val convolutionMatrices = Util.getNeighbors(image, radius)
    val convolutionImage = convolutionMatrices.map {
      matricesLine => matricesLine.map {
        matrix => matrix.flatten.zipAll(kernel.flatten, 0.0, 0.0).map {
          case (a, b) => a * b
        }.sum
      }
    }
    convolutionImage
  }


  def moduloPascal(m: Integer, funct: Integer => Pixel, size: Integer): Image = {
    def pascalTriangle (): List[List[Int]] = {
      def auxPascalTriangle (acc: List[List[Int]], n: Int): List[List[Int]] = {
        if (n == size) acc
        else
          if (n == 0) auxPascalTriangle(List(List(1)), 1)
          else {
          val prevRow = acc.head
          val newRow = (0 +: prevRow, prevRow :+ 0).zipped.map((a, b) => (a + b) % m)
          auxPascalTriangle(newRow :: acc, n + 1)
        }
      }
      auxPascalTriangle(Nil, 0).reverse
    }
    val coloredPascalTriangle = pascalTriangle().map(row => row.padTo(size, 4)).map {
      line =>
        line.map {
          number => funct(number)
        }
    }
    coloredPascalTriangle
  }



}
