package com.nsinha.problems.GoogleJam.ZeroEight.WorldFinal.packageA

import org.scalatest.FunSuite

import scala.collection.mutable

/** Created by nsinha on 3/22/17.
  */

case class ProblemA(n : Int, input : String) {
  type ASize = Int
  type BSize = Int
  type CSize = Int
  type CandidateNum = Int
  var ops = 0

  case class CandidateProps(a : ASize, b : BSize, c : CSize)
  def minA = aSortedArray(0)
  def minB = bSortedArray(0)
  def minC = cSortedArray(0)
  def maxA = aSortedArray(aSortedArray.size - 1)
  def maxB = bSortedArray(bSortedArray.size - 1)
  def maxC = cSortedArray(cSortedArray.size - 1)
  var maxFilled = 0

  var aSortedArray = mutable.MutableList[ASize]()
  var bSortedArray = mutable.MutableList[BSize]()
  var cSortedArray = mutable.MutableList[CSize]()

  val parentA = mutable.Map[CandidateNum, CandidateNum]()
  val parentB = mutable.Map[CandidateNum, CandidateNum]()

  val childrenA = mutable.Map[CandidateNum, CandidateNum]()
  val childrenB = mutable.Map[CandidateNum, CandidateNum]()

  val candidateMap = mutable.Map[CandidateNum, CandidateProps]()
  var axisDeltaA : Map[ASize, Map[CSize, Map[BSize, List[CandidateNum]]]] = _
  var axisDeltaB : Map[BSize, Map[CSize, Map[ASize, List[CandidateNum]]]] = _
  var table = Map[CSize, List[CandidateNum]]()

  solve

  def solve : Int = {
    createMapOfTuples
    createDeltas
    fillDpWhileTrackingMaxDriver(Option(minA), Option(minB))
    println(maxFilled)
    maxFilled
  }

  def createMapOfTuples = {
    val inputLines = input.split("\n")
    var index = 0
    inputLines foreach { line ⇒
      val allNumbersAsString = line.split(" ")
      val allNumbers = allNumbersAsString map (Integer.parseInt(_))
      candidateMap += index → CandidateProps(allNumbers(0), allNumbers(1), allNumbers(2))
      index = index + 1
    }
  }

  def createDeltas = {
    val axisDeltaATemp = mutable.Map[ASize, mutable.Map[CSize, mutable.Map[BSize, mutable.MutableList[CandidateNum]]]]()
    val axisDeltaBTemp = mutable.Map[BSize, mutable.Map[CSize, mutable.Map[ASize, mutable.MutableList[CandidateNum]]]]()

    Range(0, n) foreach { candNum : CandidateNum ⇒
      val candidate = candidateMap(candNum)

      val aSize = candidate.a
      val bSize = candidate.b
      val cSize = candidate.c

      if (!aSortedArray.contains(aSize)) aSortedArray += aSize
      if (!bSortedArray.contains(bSize)) bSortedArray += bSize
      if (!cSortedArray.contains(cSize)) cSortedArray += cSize

      if (!axisDeltaATemp.contains(aSize)) {
        axisDeltaATemp(aSize) = mutable.HashMap[CSize, mutable.Map[BSize, mutable.MutableList[CandidateNum]]]()
      }
      if (!axisDeltaBTemp.contains(bSize)) {
        axisDeltaBTemp(bSize) = mutable.HashMap[CSize, mutable.Map[ASize, mutable.MutableList[CandidateNum]]]()
      }

      val aMap = axisDeltaATemp(aSize)
      if (!aMap.contains(cSize)) {
        aMap += (cSize → mutable.Map[BSize, mutable.MutableList[CandidateNum]]())
      }
      if (!aMap(cSize).contains(bSize)) {
        aMap(cSize) += (bSize → mutable.MutableList[CandidateNum]())
      }
      aMap(cSize)(bSize) += candNum

      val bMap = axisDeltaBTemp(bSize)

      if (!bMap.contains(cSize)) {
        bMap += (cSize → mutable.Map[ASize, mutable.MutableList[CandidateNum]]())
      }

      if (!bMap(cSize).contains(aSize)) {
        bMap(cSize) += (aSize → mutable.MutableList[CandidateNum]())
      }
      bMap(cSize)(aSize) += candNum
    }

    axisDeltaA = {
      axisDeltaATemp map { x ⇒
        x._1 → {
          x._2 map { y ⇒
            y._1 → {
              y._2 map { z ⇒
                z._1 → z._2.toList
              }
            }.toMap
          }
        }.toMap
      }
    }.toMap

    axisDeltaB = {
      axisDeltaBTemp map { x ⇒
        x._1 → {
          x._2 map { y ⇒
            y._1 → {
              y._2 map { z ⇒
                z._1 → z._2.toList
              }
            }.toMap
          }
        }.toMap
      }
    }.toMap

    aSortedArray = aSortedArray.sorted
    bSortedArray = bSortedArray.sorted
    aSortedArray zip aSortedArray.drop(1) foreach { p ⇒
      parentA += (p._2 → p._1)
      childrenA += (p._1 → p._2)
    }
    bSortedArray zip bSortedArray.drop(1) foreach { p ⇒
      parentB += (p._2 → p._1)
      childrenB += (p._1 → p._2)
    }
  }

  def fillDpWhileTrackingMaxInRowCol(aOpt : Option[ASize], bOpt : Option[BSize]) : Unit = {
    //everytime fill the neighbors
    //returns when cant go further
    aOpt match {
      case None ⇒ return
      case _    ⇒
    }
    bOpt match {
      case None ⇒ return
      case _    ⇒
    }
    val a = aOpt.get
    val b = bOpt.get

    val cMax = 10000 - a - b

    val aMinusC = if (axisDeltaA.contains(a)) {
      ops = ops + axisDeltaA(a).size
      axisDeltaA(a) filter (x ⇒ x._1 <= cMax)
    }
    else {
      Map[CSize, Map[BSize, List[CandidateNum]]]()
    }
    val aMinusCMinusBSingleList = {
      aMinusC map { x ⇒
        //create a single list by appending all the b list that are under or equal to the bsize
        val z = {
          x._2 filter (y ⇒ y._1 <= b)
        }.foldLeft(List[CandidateNum]()) { (Z, el) ⇒
          ops = ops + 1
          Z ++ el._2
        }
        x._1 → z
      }
    } filter (x ⇒ x._2.size > 0)

    val aMinusBminusC = if (a > minA) {
      ops = ops + table.size
      table filter (x ⇒ x._1 <= cMax)
    }
    else {
      Map[CSize, List[CandidateNum]]()
    }
    val mergedMap = mergeMaps(aMinusBminusC, aMinusCMinusBSingleList)
    changeMaxFilled(mergedMap)
    table = mergedMap
    //println(s"a,b = $a,$b  ops = $ops")
  }

  def fillDpWhileTrackingMaxDriver(aOpt : Option[ASize], bOpt : Option[BSize]) : Unit = {
    bSortedArray foreach { b ⇒
      //println(b)
      aSortedArray foreach ({ a ⇒
        fillDpWhileTrackingMaxInRowCol(Option(a), Option(b))
      })
    }
  }

  def changeMaxFilled(mp : Map[CSize, List[Int]]) = {
    val cnt = mp.foldLeft(0) { (Z, el) ⇒ el._2.size + Z }
    if (maxFilled < cnt) {
      maxFilled = cnt
      //println(s"maxfilled = $maxFilled")
    }
  }

  def mergeLists(aList : List[Int], bList : List[Int]) : mutable.MutableList[Int] = {
    //assuming these lists are sorted:
    val aListMutable = aList.foldLeft(mutable.MutableList[Int]()) { (Z, el) ⇒ Z += el }
    val bListMutable = bList.foldLeft(mutable.MutableList[Int]()) { (Z, el) ⇒ Z += el }
    val mergeListRes = mutable.MutableList[Int]()

    while (aListMutable.nonEmpty & bListMutable.nonEmpty) {
      val aHead = aListMutable.head
      val bHead = bListMutable.head

      if (aHead == bHead) {
        mergeListRes += aHead
        aListMutable.drop(1)
        aListMutable.drop(1)
      }

      if (aHead < bHead) {
        aListMutable.drop(1)
        mergeListRes += aHead
      }

      if (aHead > bHead) {
        bListMutable.drop(1)
        mergeListRes += bHead
      }
    }
    aListMutable foreach (x ⇒ mergeListRes += x)
    bListMutable foreach (x ⇒ mergeListRes += x)
    mergeListRes
  }

  def mergeMaps(aMap : Map[CSize, List[Int]], bMap : Map[CSize, List[Int]]) : Map[CSize, List[Int]] = {
    if (aMap.size == 0) return bMap
    if (bMap.size == 0) return aMap

    val mergeMap = mutable.HashMap[CSize, List[Int]]()
    val visitedSet = mutable.Set[CSize]()

    for (aIter ← aMap) {
      visitedSet += aIter._1
      val aList = aIter._2
      val mergedList = if (bMap.contains(aIter._1)) {
        val bList = bMap(aIter._1)
        //ops = ops + 1
        aList ++ bList
      }
      else aList
      mergeMap += (aIter._1 → mergedList)
    }

    for (bIter ← bMap if !visitedSet.contains(bIter._1)) {
      visitedSet += bIter._1
      //ops = ops + 1
      mergeMap += bIter
    }

    mergeMap.toMap.filter(x ⇒ x._2.nonEmpty)
  }
}

class Testing extends FunSuite {
  test ("a") {
    val prob = ProblemA(
      5,
      """0 1250 0
        |3000 0 3000
        |1000 1000 1000
        |2000 1000 2000
        |1000 3000 2000""".stripMargin
    )
  }

  test ("b") {
    val prob = ProblemA(
      959,
      """1133 4204 2972
 |1290 3858 711
 |2222 1598 3480
 |1131 6142 1026
 |2064 3148 2555
 |3641 1947 2640
 |4842 1462 689
 |4514 3047 328
 |1424 958 1803
 |94 4970 2907
 |1434 5383 1552
 |10 8213 85
 |740 5966 701
 |3418 1368 2054
 |1318 1761 2873
 |1359 2365 3397
 |1668 6616 8
 |4473 3862 34
 |3775 1779 1083
 |3664 1280 2768
 |641 6266 1377
 |3067 967 2007
 |2496 4346 648
 |5427 2621 170
 |1206 2308 805
 |361 3907 3006
 |2666 3813 933
 |4780 1211 439
 |2654 5013 680
 |1074 6478 66
 |5171 2414 61
 |3587 3991 53
 |1870 2712 1036
 |635 245 891
 |3946 3460 763
 |1094 5141 1895
 |5561 1407 921
 |5441 1098 926
 |2315 4730 921
 |4517 3589 124
 |975 5378 1818
 |2965 3030 202
 |1981 5686 614
 |4347 1380 1971
 |2313 4316 685
 |4107 78 1885
 |1247 510 1115
 |984 4662 2692
 |3533 1839 148
 |1505 2095 4583
 |1213 3863 2287
 |1975 1688 3374
 |2599 5357 53
 |5580 1600 680
 |2249 5020 189
 |3651 1779 1852
 |1634 4748 246
 |4446 2698 994
 |119 1451 2904
 |2475 2009 3602
 |2744 5259 314
 |517 6769 223
 |4964 2938 88
 |3855 3490 790
 |2085 5179 1035
 |4926 3036 79
 |2306 4807 1194
 |1113 5191 753
 |2528 958 919
 |1960 541 833
 |2320 2349 1893
 |1932 4707 621
 |829 6585 950
 |992 716 2950
 |2511 5771 46
 |3442 2166 1788
 |3632 4187 285
 |71 7999 241
 |4909 731 286
 |4932 1342 1943
 |36 34 2008
 |5537 807 185
 |2399 799 731
 |4391 3460 349
 |342 5910 345
 |4994 2310 909
 |5250 2265 581
 |4069 3475 464
 |4574 3651 118
 |2625 1655 3290
 |2023 1596 1463
 |2864 1156 3832
 |4056 2077 1235
 |2330 2936 1405
 |5518 2811 29
 |3213 3095 776
 |1667 4659 1885
 |3188 2655 709
 |4631 0 2530
 |4267 1787 2171
 |4055 1170 1958
 |2059 1799 1785
 |211 2898 2014
 |665 5004 2071
 |3089 3540 678
 |1479 5415 514
 |602 6592 889
 |4091 710 2514
 |2176 5994 37
 |4025 3216 1132
 |472 2730 1551
 |2721 2030 3608
 |5141 950 1701
 |293 5831 2154
 |5058 17 1380
 |1704 759 5546
 |3460 365 1783
 |151 6842 1361
 |107 2028 1776
 |4966 1820 17
 |4864 1071 1466
 |4474 981 863
 |5336 1227 439
 |2990 4522 685
 |5348 1412 1066
 |1181 2016 3411
 |1934 5337 1102
 |3691 3365 501
 |4058 4229 46
 |4734 2102 1459
 |4403 856 2022
 |2862 3069 1872
 |2174 4549 644
 |5198 1157 959
 |1434 6004 288
 |3814 3556 594
 |1042 2434 302
 |4889 3218 190
 |2694 4977 586
 |3544 3175 211
 |4555 2234 1186
 |1943 473 1511
 |5337 2470 400
 |3128 4263 348
 |2518 4132 81
 |2983 2769 2145
 |644 6786 700
 |1609 6206 541
 |450 5709 1879
 |4182 1729 2156
 |5278 1516 113
 |4346 2511 385
 |1603 5250 943
 |2384 1664 2545
 |5422 1541 844
 |4900 1042 1150
 |3324 3686 376
 |5279 545 2344
 |639 4094 2664
 |3022 4347 378
 |409 1092 5564
 |1464 3989 2850
 |534 5404 1168
 |4050 1774 2242
 |3112 2390 2006
 |3412 2892 624
 |4507 3554 134
 |3181 3623 1039
 |67 939 699
 |4766 1708 1094
 |5399 967 1847
 |5198 1260 752
 |3172 2623 1594
 |4658 1634 1502
 |4269 4001 56
 |1872 2378 2395
 |3104 546 1460
 |4347 3075 479
 |1631 1271 2844
 |3639 3957 406
 |2062 4273 796
 |3503 3388 817
 |1142 5312 979
 |2166 2280 458
 |443 3388 700
 |4328 1683 1402
 |4624 3733 10
 |395 587 1530
 |1181 5403 1106
 |4688 1665 1440
 |1445 4014 2056
 |4356 694 1199
 |5043 731 1881
 |2717 5306 207
 |2753 4030 785
 |2680 2233 2607
 |2978 4760 32
 |5504 166 2143
 |4922 150 309
 |1300 3722 820
 |729 2870 314
 |3027 3692 839
 |3550 165 3274
 |4146 3525 242
 |5217 235 123
 |5447 1958 768
 |2454 1754 688
 |1989 3018 3079
 |169 3174 3599
 |2599 3677 279
 |428 1397 300
 |1301 5138 1454
 |206 3433 1574
 |2545 3778 796
 |641 2939 4399
 |522 4808 1880
 |2258 4790 5
 |4892 3419 21
 |3234 4725 82
 |1118 5064 1049
 |3229 1510 2120
 |2498 1170 862
 |1956 4069 379
 |3713 1557 2552
 |3579 1005 536
 |46 397 3079
 |56 3522 4072
 |3653 572 2849
 |5176 636 124
 |3963 2168 792
 |2640 1538 496
 |4590 0 2838
 |345 786 6472
 |1690 57 2274
 |281 1260 5771
 |2661 3876 570
 |4434 3543 273
 |1148 7212 14
 |3025 3611 1644
 |4651 1934 871
 |1717 773 3327
 |2886 4793 677
 |5200 2787 285
 |1959 5490 809
 |3941 2619 240
 |623 1047 965
 |758 7075 134
 |5283 3066 19
 |4989 263 1196
 |463 5422 1084
 |3704 3140 340
 |4128 3215 535
 |5557 750 557
 |1432 4032 1937
 |3983 3162 324
 |4017 840 2930
 |253 2803 4307
 |984 6921 348
 |2955 3769 706
 |1499 5867 408
 |1907 869 1656
 |1235 4485 2318
 |5399 735 1531
 |2252 2381 1567
 |1718 3473 2056
 |4080 1563 1433
 |2156 515 5484
 |1707 2225 2841
 |5204 391 2567
 |3183 1238 2010
 |4270 3012 538
 |1949 851 1122
 |3082 1281 1953
 |1702 3088 2163
 |4028 423 1837
 |5485 2731 0
 |5371 827 1875
 |187 1394 1955
 |2124 1692 3077
 |3682 4429 60
 |2388 4634 248
 |1949 1661 2940
 |911 6481 582
 |572 7039 126
 |2363 2017 2793
 |4687 3548 73
 |1717 3765 1491
 |1943 412 4776
 |763 7314 84
 |1135 3528 1929
 |4558 2786 823
 |3096 3532 1156
 |3569 2417 1431
 |211 3376 4424
 |2180 4977 1204
 |1356 332 1717
 |1819 3996 2028
 |142 5807 1435
 |682 3309 3839
 |4387 952 1978
 |1948 4092 46
 |4543 3624 170
 |2955 726 4419
 |2919 3388 957
 |1876 5900 403
 |2531 5664 97
 |1854 5738 89
 |5216 1557 161
 |2963 4616 192
 |2804 4743 121
 |2837 4867 265
 |2518 909 2146
 |3797 575 561
 |3327 4208 349
 |3006 2837 2527
 |940 5420 555
 |489 1469 2846
 |439 6960 683
 |2733 3131 2387
 |5203 2645 288
 |2075 5158 125
 |4817 192 1650
 |4033 2243 1356
 |4621 3098 211
 |1206 845 3142
 |2878 1507 1309
 |1152 5928 709
 |3431 3183 306
 |15 1571 428
 |2754 5399 184
 |1935 71 830
 |2441 1691 2605
 |4557 3668 9
 |1396 501 1186
 |1462 6026 23
 |2372 4672 225
 |2399 3641 1088
 |2074 3417 1810
 |5004 2072 870
 |1661 2690 1977
 |433 122 7216
 |5484 972 1526
 |1257 6294 9
 |5365 2944 53
 |3063 1413 314
 |4265 3435 471
 |71 6802 1386
 |2138 4707 286
 |5322 72 2002
 |3841 1715 1830
 |1573 2110 2250
 |3245 3121 369
 |1182 1142 732
 |742 2827 178
 |5224 126 890
 |407 4053 296
 |1662 3199 452
 |3235 3197 1366
 |4298 209 2558
 |608 6225 575
 |504 4941 945
 |1663 6049 314
 |943 6389 618
 |2903 1965 1872
 |4680 2669 691
 |2153 5999 185
 |4604 1473 1
 |3265 4867 217
 |1537 3201 906
 |4425 1884 814
 |3747 116 401
 |5523 2200 47
 |4069 561 1129
 |3294 1460 1583
 |1654 6608 5
 |4863 900 117
 |437 6781 521
 |3871 1627 2436
 |3204 1457 2284
 |2434 5813 68
 |1144 3020 1585
 |907 3507 685
 |713 4260 1272
 |4394 3270 358
 |200 5836 1361
 |266 6181 504
 |2065 3378 381
 |1677 273 1903
 |2338 5558 324
 |2763 1015 1328
 |4885 965 1396
 |3626 2459 741
 |1975 3548 1916
 |2202 3275 1066
 |299 1032 1648
 |2274 1758 125
 |438 1573 5744
 |3654 583 2052
 |1088 7043 98
 |4122 3429 289
 |1754 4713 396
 |3316 717 2624
 |4490 1918 18
 |5085 1959 752
 |1490 1517 154
 |4915 2764 614
 |2151 2228 742
 |4801 2786 210
 |2637 961 2311
 |3121 3719 376
 |4430 2960 917
 |2124 5941 216
 |4020 1769 393
 |5248 2527 399
 |361 6637 1044
 |724 1055 2111
 |1023 1197 1191
 |388 7378 422
 |1824 1143 1093
 |758 792 1428
 |2607 257 3516
 |1087 5312 1820
 |1151 5714 28
 |4776 3438 140
 |4322 1421 109
 |4347 164 983
 |2867 3400 952
 |318 464 6196
 |1681 3567 1815
 |3788 1244 2623
 |918 1851 2061
 |1736 5936 311
 |2649 3638 1789
 |956 1608 379
 |1744 5741 280
 |2244 4044 560
 |465 3055 4334
 |1209 3519 1008
 |5020 3111 220
 |2043 2625 2201
 |2310 5763 64
 |4669 2628 1067
 |77 59 6432
 |5046 816 2383
 |2492 298 2201
 |3944 1361 2791
 |4542 1183 2604
 |1052 5564 1389
 |771 6134 1383
 |4198 1214 2892
 |2252 1036 2433
 |4447 1875 450
 |2030 1856 3564
 |1737 4677 920
 |5410 546 1275
 |624 2991 1914
 |2893 902 1382
 |5243 866 969
 |3733 2585 1115
 |1371 2107 1152
 |1947 2906 662
 |3445 3104 634
 |2020 5753 126
 |4579 2185 1270
 |630 955 46
 |433 1917 4177
 |3302 4616 239
 |2793 1561 2175
 |2766 4986 139
 |2659 2162 3305
 |3190 4948 154
 |2413 1975 3212
 |739 162 7323
 |5154 2723 397
 |2628 1083 3658
 |3888 3646 684
 |1713 5946 587
 |3295 4765 253
 |4103 3562 48
 |1975 189 2558
 |4043 3023 361
 |2719 4385 476
 |3774 1025 1269
 |4981 589 513
 |1169 3338 854
 |4657 1537 1833
 |4902 2212 736
 |541 4682 1321
 |3727 3406 660
 |115 7586 262
 |4897 2131 672
 |4593 89 1799
 |2650 1894 2821
 |2414 2418 227
 |2881 5394 26
 |423 5086 1444
 |5317 993 1676
 |1182 3775 639
 |653 1460 4016
 |3563 2505 1490
 |4337 1009 2073
 |1910 901 2191
 |1344 1355 1298
 |3379 4921 54
 |3329 4590 197
 |2496 5401 38
 |1839 4646 1104
 |3069 3296 937
 |3900 2131 274
 |5503 2341 32
 |2944 5092 72
 |1594 4289 819
 |5497 134 960
 |3002 3929 159
 |3350 629 1517
 |3451 2464 1631
 |2379 582 320
 |2104 123 3554
 |1841 5108 889
 |1062 5461 1590
 |5466 2044 346
 |265 2023 1789
 |1222 5222 1909
 |4077 1384 1838
 |4054 2463 320
 |1387 6825 97
 |4730 2906 737
 |2654 5484 48
 |3249 1230 2261
 |1586 3289 1131
 |3878 2604 908
 |1973 2500 1358
 |430 3164 1098
 |4853 2727 370
 |4520 3654 192
 |4087 4222 62
 |467 5165 562
 |2408 2499 1200
 |1815 1034 2714
 |1260 5818 470
 |3317 3169 1428
 |5347 1873 412
 |3361 289 1705
 |5508 113 1242
 |2298 4777 733
 |3992 539 3487
 |2874 668 4555
 |1325 3257 1470
 |4096 3250 498
 |1866 4279 1367
 |5538 2358 40
 |2221 5678 389
 |696 2975 2984
 |580 2613 3272
 |3297 1175 483
 |4388 640 2255
 |5239 2841 237
 |4997 2191 916
 |2057 2054 1129
 |3930 2370 1196
 |3683 3591 265
 |3872 3259 1144
 |4859 3336 42
 |170 4224 481
 |1562 1962 2328
 |3718 3933 479
 |5140 2083 837
 |2037 5968 17
 |3728 1845 594
 |3795 2118 1166
 |1889 417 67
 |4 4241 1344
 |1877 1140 489
 |3964 1523 62
 |1929 5415 592
 |722 7259 4
 |1685 6039 330
 |3434 1958 454
 |4363 1037 1604
 |1659 245 2653
 |4097 115 1942
 |1791 4540 877
 |2546 2526 2839
 |2800 1856 831
 |4459 3756 150
 |3913 4251 67
 |1292 4808 260
 |4485 3576 195
 |1738 5318 484
 |2021 5455 560
 |2352 1423 859
 |2160 98 3929
 |1059 2309 1761
 |3907 2772 1114
 |1034 6477 402
 |3844 4146 0
 |1956 2269 3966
 |512 1789 1083
 |4199 1057 938
 |4255 2134 949
 |217 1670 5160
 |5142 2980 176
 |4935 2860 286
 |146 4832 207
 |326 2755 2372
 |3146 1801 1920
 |811 6448 213
 |3524 4539 120
 |618 3549 426
 |717 5390 1805
 |2547 552 5273
 |4620 2295 1076
 |3145 3970 330
 |1994 2546 662
 |5292 636 1856
 |2075 2099 2012
 |513 1287 1487
 |4550 2431 123
 |595 3434 1160
 |4205 3367 504
 |651 923 851
 |4777 2624 205
 |3321 241 763
 |2924 3953 438
 |3237 3449 438
 |5294 2879 7
 |1772 6575 0
 |1537 5291 520
 |845 2829 4335
 |3893 870 2076
 |3770 1925 1175
 |2022 4465 1043
 |857 6160 229
 |2560 2295 2822
 |4346 39 2489
 |957 5526 1887
 |2449 5397 397
 |2726 2851 97
 |1053 3198 853
 |3635 2625 1404
 |2671 2815 2785
 |4969 751 37
 |5306 670 514
 |5502 882 1426
 |3295 49 1046
 |1043 2042 4939
 |5082 1186 1540
 |499 2559 1730
 |4337 2531 1462
 |4701 2160 930
 |2862 2716 1986
 |5057 3079 152
 |4895 2165 153
 |4092 901 2984
 |4245 413 355
 |4336 3617 159
 |5399 2555 256
 |2300 3683 2073
 |551 6859 778
 |3931 3479 693
 |908 4849 2068
 |837 1603 5194
 |1482 6453 202
 |5026 584 495
 |1537 3625 1476
 |4922 37 279
 |3646 1108 2021
 |1481 5712 53
 |5415 2695 10
 |2401 4182 468
 |1657 1745 2311
 |1598 6574 61
 |1610 3838 1360
 |92 6642 1052
 |5195 1598 1304
 |3467 3348 1134
 |1534 2026 1843
 |4478 3042 757
 |2959 598 1240
 |1312 5341 27
 |4241 788 2358
 |1832 4522 1732
 |2078 1168 420
 |1356 5052 1743
 |3759 1877 2004
 |3692 602 1817
 |5492 2114 702
 |2067 516 263
 |3950 4218 78
 |527 7563 153
 |1312 5891 1084
 |306 4370 3491
 |2322 1114 2750
 |1934 5784 256
 |4167 2022 508
 |2044 1158 4942
 |488 5033 659
 |1819 3459 283
 |3262 1722 1731
 |4648 959 870
 |5447 1972 674
 |407 6261 459
 |685 3450 1935
 |5242 572 1354
 |4328 3520 333
 |4465 179 2456
 |4861 1724 849
 |143 5105 330
 |1697 4566 93
 |1318 1727 4820
 |317 6236 489
 |4261 2632 1109
 |3817 4013 208
 |3713 3254 412
 |2653 4262 829
 |4116 3595 531
 |3560 81 35
 |3546 1273 1355
 |4815 1912 1420
 |1476 2411 3267
 |2292 461 5167
 |341 6462 562
 |2840 1979 3326
 |3593 639 459
 |3209 1469 2177
 |2924 3845 793
 |3456 296 737
 |2362 5080 42
 |1459 1035 2702
 |4010 466 1435
 |4824 2066 1305
 |2439 1057 636
 |5496 376 1595
 |2189 1375 176
 |411 1989 1498
 |1840 463 3086
 |2424 5303 251
 |4093 2282 195
 |80 6973 1089
 |318 4307 1431
 |4799 154 953
 |1067 4189 1834
 |1902 2220 2434
 |2603 1886 1318
 |1300 5978 753
 |1974 340 5427
 |3156 3101 950
 |4149 738 86
 |4242 1699 538
 |1341 691 5332
 |5157 2756 50
 |1234 1210 4933
 |5337 956 56
 |1949 1449 981
 |4506 1485 625
 |791 3618 1436
 |4897 1105 1465
 |4856 1972 442
 |96 2835 3123
 |1918 5358 408
 |4492 845 1412
 |4124 3012 211
 |1930 5781 495
 |474 2656 4256
 |1944 5728 304
 |2495 4411 695
 |3966 1407 993
 |4526 3390 44
 |2684 2728 2308
 |4104 2515 1093
 |4847 2146 1289
 |5453 2135 217
 |83 1576 6449
 |3131 4780 272
 |1777 1220 4342
 |1578 6251 395
 |2902 4246 1101
 |1995 1340 1712
 |76 1698 2104
 |1384 5745 376
 |1679 1819 7
 |536 4384 2861
 |515 4795 642
 |2695 5027 276
 |3443 42 3771
 |3007 2748 332
 |2180 3311 2786
 |276 4288 660
 |3476 1558 2037
 |3360 3112 699
 |4780 385 2396
 |2385 5007 492
 |2952 2186 2294
 |177 5484 1340
 |2769 2423 2345
 |1505 880 445
 |1531 1653 3466
 |1232 2980 376
 |875 4160 610
 |1820 3211 134
 |5239 1980 979
 |3486 1938 1291
 |3341 3537 536
 |628 1754 1934
 |4064 2986 127
 |0 4765 1954
 |1183 39 1962
 |4931 2960 460
 |466 5023 571
 |3290 3973 102
 |686 7564 36
 |4177 798 3149
 |3132 5198 20
 |717 165 1795
 |3886 3115 661
 |1982 1291 4958
 |4922 154 2413
 |2393 5976 2
 |1583 2446 1888
 |1707 5642 926
 |4801 2079 185
 |221 4177 2382
 |1519 6617 225
 |3131 365 2018
 |3910 2202 1342
 |1041 273 540
 |2939 2906 933
 |2617 4482 1230
 |1308 788 4134
 |5470 331 696
 |1500 3770 2532
 |1543 3143 1173
 |2180 1797 896
 |2492 824 4288
 |3108 2313 203
 |3834 140 3500
 |413 2278 4506
 |2545 3142 361
 |1061 665 4086
 |3337 2359 1804
 |2822 5207 333
 |4461 339 3089
 |2589 214 1181
 |1665 3971 1921
 |1081 6627 366
 |1790 2343 1366
 |4451 1595 1526
 |1211 5541 536
 |2981 5281 30
 |5478 2555 245
 |2176 2712 61
 |3457 3777 646
 |1029 6972 226
 |1994 575 42
 |5226 1991 677
 |819 484 6823
 |4957 2348 263
 |2506 3527 148
 |2990 2410 1167
 |2135 5369 128
 |5411 872 1425
 |4477 2351 1128
 |2202 2842 960
 |5025 1895 48
 |14 4811 397
 |2769 331 4700
 |375 3048 1722
 |4412 2114 1386
 |2001 3379 2636
 |1112 1668 1492
 |5082 1382 1876
 |1310 4378 2351
 |5424 555 1750
 |329 3646 4382
 |3117 3629 1281
 |768 3123 3787
 |1984 5153 99
 |46 6304 83
 |1753 5103 1514
 |244 6668 542
 |2323 10 2618
 |3258 608 481
 |2248 3846 805
 |2974 800 3603
 |1846 4026 1424
 |4092 3377 92
 |690 1696 3842
 |2180 4742 762
 |2552 61 1392
 |2148 3782 1738
 |170 7190 215
 |702 1479 2602
 |5292 184 62
 |2324 905 393
 |5058 201 3100
 |3666 2443 608
 |920 472 216
 |4101 4015 15
 |872 65 6258
 |1469 6374 291
 |4353 102 3687
 |508 2420 423
 |2411 2924 1140
 |4319 2376 1594
 |2304 1765 996
 |3741 1000 2900
 |883 75 6976
 |2624 561 1916
 |3426 392 306
 |3719 3078 381
 |5073 2708 191
 |4227 2726 355
 |181 5254 2202
 |768 3564 582
 |3378 3485 1085
 |16 7174 1119
 |1685 6622 30
 |839 2312 1119
 |3635 807 1009
 |134 2431 5438
 |719 6460 876
 |5559 838 457
 |4191 2779 538
 |4558 2283 1021
 |2075 6099 82
 |3127 3629 405
 |2864 4089 205
 |882 2376 4136
 |2609 1554 1454
 |3331 4308 665
 |1589 6324 175
 |1735 4294 2263
 |20 519 4802
 |4438 3706 221
 |3884 1244 2607
 |4921 2358 785
 |4099 366 2640
 |5234 2078 461
 |3760 11 3264
 |1779 596 5632
 |3011 4809 93
 |4013 2289 1415
 |3430 4094 169
 |1444 6632 213
 |147 1722 3779
 |4245 2628 612
 |2589 4734 730
 |3770 4269 75
 |3048 1715 2101
 |5380 1663 1129
 |5321 2074 954
 |655 3607 684
 |1203 6749 61
 |1314 774 2401
 |5545 36 1634
 |548 7194 522
 |617 1103 1368
 |4358 3199 236
 |1488 2351 4527""".stripMargin
    )
  }

}
