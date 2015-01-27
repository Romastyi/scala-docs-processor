package docs.processor

import play.api.libs.json._

case class Good( name: String, description: String, quantity: Int, dimension: String )
case class Client( name: String, surname: String, good: List[Good] )
case class Company( name: String )
case class Model( client: Client, company: Company )

object App {

    def main(args: Array[String]): Unit = {

        val document = new HtmlDocument(
            "{client.name}, привет!\n\n" +
            "Твои любимые товары:\n" +
            "<tr>\t{client.good:row.number}) {client.good.name} ... {client.good.quantity} {client.good.dimension} </tr>\n" +
            "\n\t{client.good:begin.number}) {client.good.name} ... {client.good.quantity} {client.good.dimension}, {client.good:end}\n" +
            "\nПока.\n" +
            "Твоя на веки, {company.name}."
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

//        val xmlModel =
//            <model>
//                <client>
//                    <name>Вася</name>
//                    <surname>Пупкин</surname>
//                    <good>
//                        <name>Хлебушек</name>
//                        <description>Хлеб черный</description>
//                        <quantity>2</quantity>
//                        <dimension>ломоть</dimension>
//                    </good>
//                    <good>
//                        <name>Маслице</name>
//                        <description>Масло сливочное</description>
//                        <quantity>100</quantity>
//                        <dimension>гр.</dimension>
//                    </good>
//                    <good>
//                        <name>Сальцо</name>
//                        <description>Сало свиное, копченое</description>
//                        <quantity>4</quantity>
//                        <dimension>шмат</dimension>
//                    </good>
//                </client>
//                <company>
//                    <name>Бабуся</name>
//                </company>
//            </model>

        implicit val goodFormat = Json.format[Good]
        implicit val clientFormat = Json.format[Client]
        implicit val companyFormat = Json.format[Company]
        implicit val modelFormat = Json.format[Model]

        val jsonModel = Json.toJson(
            new Model(new Client("Вася", "Пупкин", List(
                new Good("Хлебушек", "Хлеб черный", 2, "ломоть"),
                new Good("Маслице", "Масло сливочное", 100, "гр."),
                new Good("Сальцо", "Сало свиное, копченое", 4, "шмат")
            )), new Company("Бабуся"))
        )

        import XmlEvaluator._
        import JsonEvaluator._

//        println(document.evaluate(xmlModel).plainText)
        println("-----------------------------------------------------")
        println(document.evaluate(jsonModel).plainText)
    }
}
