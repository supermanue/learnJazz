import Harmony._
import Questions._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner



@RunWith(classOf[JUnitRunner])
class DominantsSuite extends FunSuite {
  val question = QuestionArguments(Note(0,0),0)
  val otherQuestion = QuestionArguments(Note(0,0),1)


  test("two-five ") {
    val answer = twoFives(question)._2
    assert(answer == "Dm7 / G7", "two five is not correctly implemented")
  }

  test("analyze two-five"){
    val answer = analyzeTwoFives(question)._2
    assert(answer=="ii-V", "two-five analysis is not correctly implemented")
  }

  test("dominant"){
    val answer = dominant(question)._2
    assert(answer=="C", "dominant is not correctly implemented")
  }

  test("analyze_dominants"){
    var answer = analyzeDominants(question)._2
    assert(answer=="V7", "analyze_dominants is not correctly implemented")

    answer = analyzeDominants(otherQuestion)._2
    assert(answer=="V7/II", "analyze_dominants is not correctly implemented")
  }

  test("dominant_from_note"){
    val answer = dominantFromNote(question)._2
    assert(answer=="G7", "dominant_from_note  is not correctly implemented")
  }

  test("tritone"){
    val answer = tritone(question)._2
    assert(answer=="G7, grado I", "tritone is not correctly implemented")
  }

  test("analize_tritone"){
    val answer = analizeTritone(question)._2
    assert(answer=="subV7/I (sustituto de G)", "analize_tritone  is not correctly implemented")
  }

  test("two_tritone"){
    val answer = twoTritone(question)._2
    assert(answer=="Dm7 / Db7", "two_tritone  is not correctly implemented")
  }

  test("analyze_two_tritone"){
    val answer = analyzeTwoTritone(question)._2
    assert(answer=="ii-subV7", "analyze_two_tritone  is not correctly implemented")
  }

  test("two_five_division_in_tritone"){
    val answer = twoFiveDivisionInTritone(question)._2
    assert(answer=="Am7 / Db7", "two_five_division_in_tritone  is not correctly implemented")
  }
}

@RunWith(classOf[JUnitRunner])
class ChordsSuite extends FunSuite {
  val question = QuestionArguments(Note(0, 0), 0)
  val otherQuestion = QuestionArguments(Note(0, 0), 1)


  test("ascendingTriad ") {
    val answer = ascendingTriad(question)._2
    assert(answer == "C, E, G", "ascendingTriad is not correctly implemented")
  }

  test("descendingTriad ") {
    val answer = descendingTriad(question)._2
    assert(answer == "G, E, C", "descendingTriad is not correctly implemented")
  }

  test("ascendingCuatriad Easy ") {
    val answer = ascendingCuatriad(question)._2
    assert(answer == "C, E, G, B", "ascendingCuatriad is not correctly implemented")
  }

  test("descendingCuatriad Easy ") {
    val answer = descendingCuatriad(question)._2
    assert(answer == "B, G, E, C", " is not correctly implemented")
  }

  test("ascendingCuatriad Hard ") {
    val answer = ascendingCuatriadHard(question)._2
    assert(answer == "C, E, G, B", "ascendingCuatriad is not correctly implemented")
  }

  test("descendingCuatriad Hard ") {
    val answer = descendingCuatriadHard(question)._2
    assert(answer == "B, G, E, C", " is not correctly implemented")
  }

  test("cuatriadInScale ") {
    val answer = cuatriadInMajorScale(question)._2
    assert(answer == "Cmaj7", "cuatriadInScale is not correctly implemented")
  }

  test("analyze_chord_in_Scale ") {
    val answer = analyzeChordInMajorScale(question)._2
    assert(answer == "Imaj7", " is not correctly implemented")
  }
}

  @RunWith(classOf[JUnitRunner])
  class IntervalsSuite extends FunSuite {
    val question = QuestionArguments(Note(0, 0), 3)

    test("ascendingInterval ") {
      val answer = ascendingInterval(question)._2
      assert(answer == "D", "ascendingInterval is not correctly implemented")
    }

    test("descendingInterval ") {
      val answer = descendingInterval(question)._2
      assert(answer == "Bb", "descendingInterval is not correctly implemented")
    }
  }

@RunWith(classOf[JUnitRunner])
class KeysSuite extends FunSuite {
  val question = QuestionArguments(Note(0, 0), 7)

  test("key_signature_with_sharps") {
    val answer = keySignatureWithSharps(question)._2
    assert(answer == "C# mayor: F# C# G# D# A# E# B#", "key_signature_with_sharps is not correctly implemented")
  }

  test("get_sharps_for_signature") {
    val answer = getSharpsForSignature(question)._2
    assert(answer == "F# C# G# D# A# E# B#", "get_sharps_for_signature is not correctly implemented")
  }

  test("key_signature_with_flats") {
    val answer = keySignatureWithFlats(question)._2
    assert(answer == "Cb mayor: Bb Eb Ab Db Gb Cb Fb", "key_signature_with_flats is not correctly implemented")
  }

  test("get_flats_for_signature") {
    val answer = getFlatsForSignature(question)._2
    assert(answer == "Bb Eb Ab Db Gb Cb Fb", "get_flats_for_signature is not correctly implemented")
  }


}

  @RunWith(classOf[JUnitRunner])
  class ScalesSuite extends FunSuite {
    val question = QuestionArguments(Note(0, 0), 0)

    test("modeNotes ") {
      val answer = modeNotes(question)._2
      assert(answer == "C, D, E, F, G, A, B", "modeNotes is not correctly implemented")
    }

    test("modeTensions ") {
      val answer = modeTensions(question)._2
      assert(answer == "D, A", "modeTensions is not correctly implemented")
    }

    test("modeForbiddenNotes ") {
      val answer = modeForbiddenNotes(question)._2
      assert(answer == "F", "modeForbiddenNotes is not correctly implemented")
    }

    test("modeHarmonization ") {
      val answer = modeHarmonization(question)._2
      assert(answer == "Cmaj7, Dm7, Em7, Fmaj7, G7, Am7, Bm7b5", "modeHarmonization is not correctly implemented")
    }

    test("scaleFromSecondaryDominant ") {
      val answer = scaleFromSecondaryDominant(question)._2
      assert(answer == "C, D, D#, Eb, F, G, A, Bb; C, Db, D#, E, Fb, G, A, Bb", "scaleFromSecondaryDominant is not correctly implemented")
    }

    test("scaleFromTritoneSubstitution ") {
      val answer = scaleFromTritoneSubstitution(question)._2
      assert(answer == "Eb, Fb, G, A, Bb, C, D", "scaleFromTritoneSubstitution is not correctly implemented")
    }
  }

    @RunWith(classOf[JUnitRunner])
    class TonalAreasSuite extends FunSuite {
      val question = QuestionArguments(Note(0, 0), 0)

      test("tonalAreas ") {
        val answer = tonalArea(question)._2
        assert(answer == "Cmaj7, Em7, Am7", "tonalArea is not correctly implemented")
      }
}
