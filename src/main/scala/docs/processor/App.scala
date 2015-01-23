package docs.processor

object App {

    def main(args: Array[String]): Unit = {
        implicit object mySyntax extends Syntax( start = "{{", finish = "}}", separator = "->", qualifier = "::" )
//        implicit val mySyntax = new DefaultSyntax()
//        implicit val myValidator = new XmlValidator(
//            <keywords>
//                <group name="w" repeatable="yes">
//                    <group name="w1" repeatable="no">
//                        <word name="Hello" repeatable="yes"/>
//                    </group>
//                    <word name="world"/>
//                </group>
//            </keywords>)
        implicit val myValidator = new JsonValidator(
            """[
              | {
              |  "group": {
              |    "name": "w",
              |    "repeatable": "yes",
              |    "child": [
              |     {
              |      "group": {
              |        "name": "w1",
              |        "repeatable": "no",
              |        "child": [
              |         {
              |          "word": {
              |            "name": "Hello",
              |            "repeatable": "yes"
              |          }
              |         }
              |        ]
              |      }
              |     },
              |     {
              |      "word": {
              |        "name": "world"
              |      }
              |     }
              |    ]
              |  }
              | }
              |]""".stripMargin)
        Parser.parse("{{w->w1->Hello->avg::row.number}}, {{w::row}}").foreach { case (key, value) =>
            println(key + " (" + value.t + ")")
        }
    }
}
