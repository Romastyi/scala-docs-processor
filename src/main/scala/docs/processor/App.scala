package docs.processor

object App {

    def main(args: Array[String]): Unit = {

        val document = new TextDocument(
            "{client.name}, привет!\n\n" +
            "Твой любимые товары:\n" +
            "\t{client.good.name:row.number} ... {client.good.quantity} {client.good.dimension}\n" +
            "\nПока.\n" +
            "Твой на веки, {company.name}."
        )

        implicit val defaultSyntax = new DefaultSyntax()
//        implicit object mySyntax extends Syntax( start = "{{", finish = "}}", separator = "->", qualifier = "::" )
        implicit val xmlValidator = new XmlValidator(
            <keywords>
                <group name="client" model="client">
                    <word name="surname" field="surname"/>
                    <word name="name" field="name"/>
                    <group name="good" model="good" repeatable="yes">
                        <word name="name" field="name"/>
                        <word name="description" field="description"/>
                        <word name="quantity" field="quantity"/>
                        <word name="dimension" field="dimension"></word>
                    </group>
                </group>
                <group name="company" model="company">
                    <word name="name" field="name"/>
                </group>
            </keywords>
        )

//        implicit val jsonValidator = new JsonValidator(
//            """[
//              | {
//              |  "group": {
//              |    "name": "w",
//              |    "repeatable": "yes",
//              |    "child": [
//              |     {
//              |      "group": {
//              |        "name": "w1",
//              |        "repeatable": "no",
//              |        "child": [
//              |         {
//              |          "word": {
//              |            "name": "Hello",
//              |            "repeatable": "yes"
//              |          }
//              |         }
//              |        ]
//              |      }
//              |     },
//              |     {
//              |      "word": {
//              |        "name": "world"
//              |      }
//              |     }
//              |    ]
//              |  }
//              | }
//              |]""".stripMargin
//        )

        val model =
            <client>
                <name>Вася</name>
                <surname>Пупкин</surname>
                <good>
                    <name>Хлебушек</name>
                    <description>Хлеб черный</description>
                    <quantity>2</quantity>
                    <dimension>ломоть</dimension>
                </good>
                <good>
                    <name>Маслице</name>
                    <description>Масло сливочное</description>
                    <quantity>100</quantity>
                    <dimension>гр.</dimension>
                </good>
                <good>
                    <name>Сальцо</name>
                    <description>Сало свиное, копченое</description>
                    <quantity>4</quantity>
                    <dimension>шмат</dimension>
                </good>
            </client>

        import xmlEvaluator._

        document.evaluate(model)
    }
}
