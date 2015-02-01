/**
 * Created by romastyi on 23/01/15.
 */

import docs.processor._
import org.scalatest._

class ParserSpec extends FlatSpec with Matchers {

    "Parser" should "parse keyword (default syntax, dummy validator)" in {

        implicit val syntax = new DefaultSyntax()
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

        Parser.parse("{client.surname.trim.avg}")
    }

}
