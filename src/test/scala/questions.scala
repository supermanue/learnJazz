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
    val answer = Dominants.twoFives(question)._2
    assert(answer == "Dm7 / G7", "two five is not correctly implemented")
  }

  test("analyze two-five"){
    val answer = Dominants.analyzeTwoFives(question)._2
    assert(answer=="ii-V", "two-five analysis is not correctly implemented")
  }

  test("dominant"){
    val answer = Dominants.dominant(question)._2
    assert(answer=="C", "dominant is not correctly implemented")
  }

  test("analyze_dominants"){
    var answer = Dominants.analyze_dominants(question)._2
    assert(answer=="V7", "analyze_dominants is not correctly implemented")

    answer = Dominants.analyze_dominants(otherQuestion)._2
    assert(answer=="V7/II", "analyze_dominants is not correctly implemented")
  }

  test("dominant_from_note"){
    val answer = Dominants.dominant_from_note(question)._2
    assert(answer=="G7", "dominant_from_note  is not correctly implemented")
  }

  test("tritone"){
    val answer = Tritones.tritone(question)._2
    assert(answer=="G7, grado I", "tritone is not correctly implemented")
  }

  test("analize_tritone"){
    val answer = Tritones.analize_tritone(question)._2
    assert(answer=="subV7/I (sustituto de G)", "analize_tritone  is not correctly implemented")
  }

  test("two_tritone"){
    val answer = Tritones.two_tritone(question)._2
    assert(answer=="Dm7 / Db7", "two_tritone  is not correctly implemented")
  }

  test("analyze_two_tritone"){
    val answer = Tritones.analyze_two_tritone(question)._2
    assert(answer=="ii-subV7", "analyze_two_tritone  is not correctly implemented")
  }

  test("two_five_division_in_tritone"){
    val answer = Tritones.two_five_division_in_tritone(question)._2
    assert(answer=="Am7 / Db7", "two_five_division_in_tritone  is not correctly implemented")
  }
}

@RunWith(classOf[JUnitRunner])
class ChordsSuite extends FunSuite {
  val question = QuestionArguments(Note(0, 0), 0)
  val otherQuestion = QuestionArguments(Note(0, 0), 1)


  test("ascendingTriad ") {
    val answer = Triads.ascendingTriad(question)._2
    assert(answer == "C, E, G", "ascendingTriad is not correctly implemented")
  }

  test("descendingTriad ") {
    val answer = Triads.descendingTriad(question)._2
    assert(answer == "G, E, C", "descendingTriad is not correctly implemented")
  }

  test("ascendingCuatriad Easy ") {
    val answer = CuatriadsEasy.ascendingCuatriad(question)._2
    assert(answer == "C, E, G, B", "ascendingCuatriad is not correctly implemented")
  }

  test("descendingCuatriad Easy ") {
    val answer = CuatriadsEasy.descendingCuatriad(question)._2
    assert(answer == "B, G, E, C", " is not correctly implemented")
  }

  test("ascendingCuatriad Hard ") {
    val answer = CuatriadsHard.ascendingCuatriad(question)._2
    assert(answer == "C, E, G, B", "ascendingCuatriad is not correctly implemented")
  }

  test("descendingCuatriad Hard ") {
    val answer = CuatriadsHard.descendingCuatriad(question)._2
    assert(answer == "B, G, E, C", " is not correctly implemented")
  }

  test("cuatriadInScale ") {
    val answer = CuatriadsInMajorScale.cuatriadInScale(question)._2
    assert(answer == "Cmaj7", "cuatriadInScale is not correctly implemented")
  }

  test("analyze_chord_in_Scale ") {
    val answer = CuatriadsInMajorScale.analyzeChordInScale(question)._2
    assert(answer == "Imaj7", " is not correctly implemented")
  }
}

  @RunWith(classOf[JUnitRunner])
  class IntervalsSuite extends FunSuite {
    val question = QuestionArguments(Note(0, 0), 3)

    test("ascendingInterval ") {
      val answer = Intervals.ascendingInterval(question)._2
      assert(answer == "D", "ascendingInterval is not correctly implemented")
    }

    test("descendingInterval ") {
      val answer = Intervals.descendingInterval(question)._2
      assert(answer == "Bb", "descendingInterval is not correctly implemented")
    }
  }

@RunWith(classOf[JUnitRunner])
class KeysSuite extends FunSuite {
  val question = QuestionArguments(Note(0, 0), 6)

  test("key_signature_with_flats ") {
    val answer = Keys.key_signature_with_flats(question)._2
    assert(answer == "B mayor", "key_signature_with_flats is not correctly implemented")
  }

  test("get_flats_for_signature ") {
    val answer = Keys.get_flats_for_signature(question)._2
    assert(answer == "F# C# G# D# A# E# B#", "get_flats_for_signature is not correctly implemented")
  }
}

  @RunWith(classOf[JUnitRunner])
  class ScalesSuite extends FunSuite {
    val question = QuestionArguments(Note(0, 0), 0)

    test("modeNotes ") {
      val answer = Scales.modeNotes(question)._2
      assert(answer == "C, D, E, F, G, A, B", "modeNotes is not correctly implemented")
    }

    test("modeTensions ") {
      val answer = Scales.modeTensions(question)._2
      assert(answer == "D, A", "modeTensions is not correctly implemented")
    }

    test("modeForbiddenNotes ") {
      val answer = Scales.modeForbiddenNotes(question)._2
      assert(answer == "F", "modeForbiddenNotes is not correctly implemented")
    }

    test("modeHarmonization ") {
      val answer = Scales.modeHarmonization(question)._2
      assert(answer == "Cmaj7, Dm7, Em7, Fmaj7, G7, Am7, Bm7b5", "modeHarmonization is not correctly implemented")
    }

    test("scaleFromSecondaryDominant ") {
      val answer = Scales.scaleFromSecondaryDominant(question)._2
      assert(answer == "C, D, D#, Eb, F, G, A, Bb; C, Db, D#, E, Fb, G, A, Bb", "scaleFromSecondaryDominant is not correctly implemented")
    }

    test("scaleFromTritoneSubstitution ") {
      val answer = Scales.scaleFromTritoneSubstitution(question)._2
      assert(answer == "Eb, Fb, G, A, Bb, C, D", "scaleFromTritoneSubstitution is not correctly implemented")
    }
  }

    @RunWith(classOf[JUnitRunner])
    class TonalAreasSuite extends FunSuite {
      val question = QuestionArguments(Note(0, 0), 0)

      test("tonalAreas ") {
        val answer = TonalAreas.tonalArea(question)._2
        assert(answer == "Cmaj7, Em7, Am7", "tonalArea is not correctly implemented")
      }
}
