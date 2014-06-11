//============================================================================//
// This file is part of the Yang software project. It is distributed under the
// MIT License. See LICENSE file for details.
//============================================================================//
#include "tests.h"

namespace yang {

const std::string TestExhaustiveStr = R"(
global {
  const t = true;
  var u = true;
  var v = 1.;
  var uu = (true, false);
  var vv = (1., 0.);
}

export tern0 = int() {return t ? t : !t;}
export tern1 = int() {return !t ? !t : t;}
export tern2 = int() {return $+((t, !t) ? (t, t) : (!t, !t));}
export tern3 = int() {return $+(t ? (t, !t) : (!t, t));}

export lor0 = int() {return t || !t;}
export lor1 = int() {return !t || t;}
export lor2 = int() {return $+(!t || (t, !t));}
export lor3 = int() {return $+((t, !t) || !t);}
export lor4 = int() {return $+((!t, !t) || (!t, t));}

export land0 = int() {return t && t;}
export land1 = int() {return !(!t && t);}
export land2 = int() {return $+(t && (t, !t));}
export land3 = int() {return $+((t, !t) && t);}
export land4 = int() {return $+((t, !t) && (t, t));}

export bor0 = int() {return t | !t;}
export bor1 = int() {return $+(!t | (t, !t));}
export bor2 = int() {return $+((t, !t) | !t);}
export bor3 = int() {return $+((!t, !t) | (t, !t));}
export band0 = int() {return t & t;}
export band1 = int() {return $+(t & (t, !t));}
export band2 = int() {return $+((t, !t) & t);}
export band3 = int() {return $+((t, t) & (t, !t));}
export bxor0 = int() {return t ^ !t;}
export bxor1 = int() {return $+(t ^ (t, !t));}
export bxor2 = int() {return $+((t, !t) ^ !t);}
export bxor3 = int() {return $+((t, t) ^ (t, !t));}

export blsh0 = int() {return (t << t) - 1;}
export blsh1 = int() {return $-(t << (t, !t));}
export blsh2 = int() {return $+((t, !t) << t) - 1;}
export blsh3 = int() {return $-((t, t) << (t, !t));}
export brsh0 = int() {return 2 >> t;}
export brsh1 = int() {return $-(2 >> (!t, t));}
export brsh2 = int() {return $+((2, !t) >> t);}
export brsh3 = int() {return $-((2, 2) >> (!t, t));}

export pow0 = int() {return t ** 2;}
export pow1 = int() {return $-(2 ** (t, !t));}
export pow2 = int() {return $+((t, !t) ** 2);}
export pow3 = int() {return $+((t, !t) ** (2, t));}
export pow4 = int() {return [t. ** 1.5];}
export pow5 = int() {return [$-(2. ** (t., 0.))];}
export pow6 = int() {return [$+((t., 0.) ** 2.)];}
export pow7 = int() {return [$+((t., 0.) ** (2., t.))];}

export mod0 = int() {return 3 % 2;}
export mod1 = int() {return $+(3 % (2, 3));}
export mod2 = int() {return $+((3, 2) % 2);}
export mod3 = int() {return $+((3, 4) % (3, 3));}
export mod4 = int() {return [t. % 2.];}
export mod5 = int() {return [$+(3. % (2., 3.))];}
export mod6 = int() {return [$+((3., 2.) % 2.)];}
export mod7 = int() {return [$+((3., 4.) % (3., 3.))];}

export add0 = int() {return t + !t;}
export add1 = int() {return $+(!t + (t, !t));}
export add2 = int() {return $+((t, !t) + !t);}
export add3 = int() {return $+((t, !t) + (!t, !t));}
export add4 = int() {return [t. + 0.];}
export add5 = int() {return [$+(0. + (t., 0.))];}
export add6 = int() {return [$+((t., 0.) + 0.)];}
export add7 = int() {return [$+((t., 0.) + (0., 0.))];}

export sub0 = int() {return t - !t;}
export sub1 = int() {return $+(t - (t, !t));}
export sub2 = int() {return $+((t, !t) - !t);}
export sub3 = int() {return $+((t, !t) - (!t, !t));}
export sub4 = int() {return [t. - 0.];}
export sub5 = int() {return [$+(t. - (t., 0.))];}
export sub6 = int() {return [$+((t., 0.) - 0.)];}
export sub7 = int() {return [$+((t., 0.) - (0., 0.))];}

export mul0 = int() {return t * t;}
export mul1 = int() {return $+(t * (t, !t));}
export mul2 = int() {return $+((t, !t) * t);}
export mul3 = int() {return $+((t, !t) * (t, !t));}
export mul4 = int() {return [t. * t.];}
export mul5 = int() {return [$+(t. * (t., 0.))];}
export mul6 = int() {return [$+((t., 0.) * t.)];}
export mul7 = int() {return [$+((t., 0.) * (t., 0.))];}

export div0 = int() {return t / t;}
export div1 = int() {return $+(t / (t, t)) - 1;}
export div2 = int() {return $+((t, !t) / t);}
export div3 = int() {return $+((t, !t) / (t, t));}
export div4 = int() {return [1.5 / 1.5];}
export div5 = int() {return [$+(t. / (2., 2.))];}
export div6 = int() {return [$+((t., 0.) / t.)];}
export div7 = int() {return [$+((t., 0.) / (t., t.))];}

export eq0 = int() {return t == t;}
export eq1 = int() {return $+(t == (t, !t));}
export eq2 = int() {return $+((t, !t) == t);}
export eq3 = int() {return $+((t, !t) == (t, t));}
export eq4 = int() {return .5 == .5;}
export eq5 = int() {return $+(t. == (t., 0.));}
export eq6 = int() {return $+((t., 0.) == t.);}
export eq7 = int() {return $+((t., 0.) == (t., t.));}

export ne0 = int() {return t != !t;}
export ne1 = int() {return $+(t != (t, !t));}
export ne2 = int() {return $+((t, !t) != t);}
export ne3 = int() {return $+((t, !t) != (t, t));}
export ne4 = int() {return .5 != 1.;}
export ne5 = int() {return $+(t. != (t., 0.));}
export ne6 = int() {return $+((t., 0.) != t.);}
export ne7 = int() {return $+((t., 0.) != (t., t.));}

export ge0 = int() {return t >= !t;}
export ge1 = int() {return $+(!t >= (!t, t));}
export ge2 = int() {return $+((t, !t) >= t);}
export ge3 = int() {return $+((t, !t) >= (t, t));}
export ge4 = int() {return 1. >= .5;}
export ge5 = int() {return $+(0. >= (t., 0.));}
export ge6 = int() {return $+((t., 0.) >= t.);}
export ge7 = int() {return $+((t., 0.) >= (t., t.));}

export le0 = int() {return !t <= t;}
export le1 = int() {return $+(t <= (t, !t));}
export le2 = int() {return $+((t, !t) <= !t);}
export le3 = int() {return $+((t, !t) <= (!t, t));}
export le4 = int() {return 1. <= 1.5;}
export le5 = int() {return $+(t. <= (t., 0.));}
export le6 = int() {return $+((t., 0.) <= 0.);}
export le7 = int() {return $+((t., 0.) <= (0., t.));}

export gt0 = int() {return t > !t;}
export gt1 = int() {return $+(t > (!t, t));}
export gt2 = int() {return $+((t, !t) > !t);}
export gt3 = int() {return $+((t, !t) > (!t, t));}
export gt4 = int() {return 1. > .5;}
export gt5 = int() {return $+(.5 > (t., 0.));}
export gt6 = int() {return $+((t., 0.) > .5);}
export gt7 = int() {return $+((t., 0.) > (t., -.5));}

export lt0 = int() {return !t < t;}
export lt1 = int() {return $+(!t < (t, !t));}
export lt2 = int() {return $+((t, !t) < t);}
export lt3 = int() {return $+((t, !t) < (!t, t));}
export lt4 = int() {return 1. < 1.5;}
export lt5 = int() {return $+(t. < (t., 2.));}
export lt6 = int() {return $+((t., 1.5) < 1.5);}
export lt7 = int() {return $+((t., 0.) < (0., t.));}

export flor0 = int() {return $||(t, t);}
export flor1 = int() {return $||(!t, !t, t);}
export fland0 = int() {return $&&(t, t);}
export fland1 = int() {return $&&(t, t, t);}

export fbor0 = int() {return $|(!t, t);}
export fbor1 = int() {return $|(t, t, t);}
export fband0 = int() {return $&(t, 5);}
export fband1 = int() {return $&(7, 3, t);}
export fbxor0 = int() {return $^(3, 2);}
export fbxor1 = int() {return $^(1, 3, 3);}

export fblsh0 = int() {return $<<(t, !t);}
export fblsh1 = int() {return $<<(t, !t, !t);}
export fbrsh0 = int() {return $>>(2, t);}
export fbrsh1 = int() {return $>>(8, 2, t);}

export fpow0 = int() {return $**(t, 2);}
export fpow1 = int() {return $**(t, 2, 2);}
export fpow2 = int() {return [$**(t., -t.)];}
export fpow3 = int() {return [$**(t., t., -t.)];}

export fmod0 = int() {return $%(t, 2);}
export fmod1 = int() {return $%(11, 6, 4);}
export fmod2 = int() {return [$%(3.5, 2.5)];}
export fmod3 = int() {return [$%(7., 4., 2.)];}

export fadd0 = int() {return $+(2, -1);}
export fadd1 = int() {return $+(-1, t, t);}
export fadd2 = int() {return [$+(.5, .5)];}
export fadd3 = int() {return [$+(.5, .25, .25)];}

export fsub0 = int() {return $-(2, t);}
export fsub1 = int() {return $-(4, 2, t);}
export fsub2 = int() {return [$-(1.5, .5)];}
export fsub3 = int() {return [$-(2., .75, .25)];}

export fmul0 = int() {return $*(t, t);}
export fmul1 = int() {return $*(t, t, t);}
export fmul2 = int() {return [$*(2., .5)];}
export fmul3 = int() {return [$*(.5, 4., .5)];}

export fdiv0 = int() {return $/(t, 1);}
export fdiv1 = int() {return $/(4, 2, 2);}
export fdiv2 = int() {return [$/(.5, .5)];}
export fdiv3 = int() {return [$/(t., 2., .5)];}

export feq0 = int() {return $==(t, t);}
export feq1 = int() {return $==(2, 2, 2);}
export feq2 = int() {return $==(1.5, 1.5);}
export feq3 = int() {return $==(t., t., t.);}

export fne0 = int() {return $!=(t, !t);}
export fne1 = int() {return $!=(2, t, 2);}
export fne2 = int() {return $!=(1.5, t.);}
export fne3 = int() {return $!=(t., 0., t.);}

export fge0 = int() {return $>=(t, !t);}
export fge1 = int() {return $>=(3, 2, 2);}
export fge2 = int() {return $>=(1.5, t.);}
export fge3 = int() {return $>=(0., (!t)., -1.);}

export fle0 = int() {return $<=(t, t);}
export fle1 = int() {return $<=(0, t, 2);}
export fle2 = int() {return $<=(.5, t.);}
export fle3 = int() {return $<=(0., 1., t.);}

export fgt0 = int() {return $>(t, !t);}
export fgt1 = int() {return $>(3, 2, 1);}
export fgt2 = int() {return $>(1.5, t.);}
export fgt3 = int() {return $>(t., 0., -1.);}

export flt0 = int() {return $<(!t, t);}
export flt1 = int() {return $<(t, 3, 5);}
export flt2 = int() {return $<(0.5, t.);}
export flt3 = int() {return $<(t., 2., 3.);}

export lneg0 = int() {return !!t;}
export lneg1 = int() {return !!!!t;}
export bneg0 = int() {return 2 + ~!t;}
export bneg1 = int() {return ~~t;}

export usub0 = int() {return -(-t);}
export usub1 = int() {return -[- - -(t.)];}
export uadd0 = int() {return +(+t);}
export uadd1 = int() {return + +[+ +(+t.)];}

export alor0 = int() {return u ||= t;}
export alor1 = int() {return $+(uu ||= (t, !t));}
export alor2 = int() {return $+(uu ||= !t);}

export aland0 = int() {return u &&= t;}
export aland1 = int() {return $+(uu &&= (t, !t));}
export aland2 = int() {return $+(uu &&= t);}

export abor0 = int() {return u |= t;}
export abor1 = int() {return $+(uu |= (t, !t));}
export abor2 = int() {return $+(uu |= !t);}

export aband0 = int() {return u &= t;}
export aband1 = int() {return $+(uu &= (21, !t));}
export aband2 = int() {return $+(uu &= 31);}

export abxor0 = int() {return u ^= !t;}
export abxor1 = int() {return $+(uu ^= (!t, !t));}
export abxor2 = int() {return $+(uu ^= !t);}

export ablsh0 = int() {return u <<= !t;}
export ablsh1 = int() {return $+(uu <<= (!t, t));}
export ablsh2 = int() {return $+(uu <<= !t);}

export abrsh0 = int() {return u >>= !t;}
export abrsh1 = int() {return $+(uu >>= (!t, t));}
export abrsh2 = int() {return $+(uu >>= !t);}

export apow0 = int() {return u **= t;}
export apow1 = int() {return $+(uu **= (!t, t));}
export apow2 = int() {return $+(uu **= 2);}
export apow3 = int() {return [v **= -.5];}
export apow4 = int() {return $+[(vv **= (!t, t).)];}
export apow5 = int() {return $+[(vv **= t.)];}

export amod0 = int() {return u %= 2;}
export amod1 = int() {return $+(uu %= (2, 3));}
export amod2 = int() {return $+(uu %= 2);}
export amod3 = int() {return [v %= 2.];}
export amod4 = int() {return $+[(vv %= (3, 2).)];}
export amod5 = int() {return $+[(vv %= 2.)];}

export aadd0 = int() {return u += !t;}
export aadd1 = int() {return $+(uu += (!t, !t));}
export aadd2 = int() {return $+(uu += !t);}
export aadd3 = int() {return [v += 0.];}
export aadd4 = int() {return $+[(vv += (!t, !t).)];}
export aadd5 = int() {return $+[(vv += 0.)];}

export asub0 = int() {return u -= !t;}
export asub1 = int() {return $+(uu -= (!t, !t));}
export asub2 = int() {return $+(uu -= !t);}
export asub3 = int() {return [v -= 0.];}
export asub4 = int() {return $+[(vv -= (!t, !t).)];}
export asub5 = int() {return $+[(vv -= 0.)];}

export amul0 = int() {return u *= t;}
export amul1 = int() {return $+(uu *= (t, !t));}
export amul2 = int() {return $+(uu *= t);}
export amul3 = int() {return [v *= 1.];}
export amul4 = int() {return $+[(vv *= (t, !t).)];}
export amul5 = int() {return $+[(vv *= t.)];}

export adiv0 = int() {return u /= t;}
export adiv1 = int() {return $+(uu /= (t, 2));}
export adiv2 = int() {return $+(uu /= t);}
export adiv3 = int() {return [v /= t.];}
export adiv4 = int() {return $+[(vv /= (t, 2).)];}
export adiv5 = int() {return $+[(vv /= t.)];}

export pfix0 = int() {++u; return --u;}
export pfix1 = int() {++uu; return $+--++--uu;}
export pfix2 = int() {return [++v - --v];}
export pfix3 = int() {++vv; return $+[--++--vv];}

export to_int0 = int() {return [v];}
export to_int1 = int() {return [vv][0];}
export to_float0 = int() {return [u.];}
export to_float1 = int() {return [uu.[0]];}
)";

TEST_F(YangTest, Exhaustive)
{
  if (!filter("exhaustive")) {
    return;
  }

  auto prog = program(TestExhaustiveStr);
  auto inst = instance(TestExhaustiveStr);
  for (const auto& pair : prog.get_functions()) {
    EXPECT_EQ(inst.call<int_t>(pair.first), 1) <<
        pair.first << " should return 1!";
  }
}

const std::string TestExhaustiveRefCountStr = R"(
fn = int()()
{
  closed var v = 0;
  closed const m = MuserType();
  return int()
  {
    m;
    return ++v;
  };
}

fn2 = int(int)(int a, int b)
{
  return int(int c)
  {
    return a + b + c;
  };
}

export exhaustive_refcount = int()
{
  const ignore = void(int) {};
  const ignoref = void(float) {};
  const ignore2 = void(int2) {};

  fn()() ? fn() : fn();
  fn()() ? fn() : fn()() ? fn() : fn();

  fn()() || fn()();
  (fn()(), fn()()) || (1, 1);
  (1, 1) || (fn()(), fn()());
  fn()() && fn()();
  (fn()(), fn()()) && (1, 1);
  (1, 1) && (fn()(), fn()());

  ignore(fn()() | fn()());
  ignore2((fn()(), fn()()) | (1, 1));
  ignore2((1, 1) | (fn()(), fn()()));
  ignore(fn()() & fn()());
  ignore2((fn()(), fn()()) & (1, 1));
  ignore2((1, 1) & (fn()(), fn()()));
  ignore(fn()() ^ fn()());
  ignore2((fn()(), fn()()) ^ (1, 1));
  ignore2((1, 1) ^ (fn()(), fn()()));
  ignore(fn()() << fn()());
  ignore2((fn()(), fn()()) << (1, 1));
  ignore2((1, 1) << (fn()(), fn()()));
  ignore(fn()() >> fn()());
  ignore2((fn()(), fn()()) >> (1, 1));
  ignore2((1, 1) >> (fn()(), fn()()));

  ignore(fn()() ** fn()());
  ignore2((fn()(), fn()()) ** (1, 1));
  ignore2((1, 1) ** (fn()(), fn()()));
  ignore(fn()() % fn()());
  ignore2((fn()(), fn()()) % (1, 1));
  ignore2((1, 1) % (fn()(), fn()()));
  ignore(fn()() + fn()());
  ignore2((fn()(), fn()()) + (1, 1));
  ignore2((1, 1) + (fn()(), fn()()));
  ignore(fn()() - fn()());
  ignore2((fn()(), fn()()) - (1, 1));
  ignore2((1, 1) - (fn()(), fn()()));
  ignore(fn()() * fn()());
  ignore2((fn()(), fn()()) * (1, 1));
  ignore2((1, 1) * (fn()(), fn()()));
  ignore(fn()() / fn()());
  ignore2((fn()(), fn()()) / (1, 1));
  ignore2((1, 1) / (fn()(), fn()()));

  ignore(fn()() == fn()());
  ignore2((fn()(), fn()()) == (1, 1));
  ignore2((1, 1) == (fn()(), fn()()));
  ignore(fn()() != fn()());
  ignore2((fn()(), fn()()) != (1, 1));
  ignore2((1, 1) != (fn()(), fn()()));
  ignore(fn()() >= fn()());
  ignore2((fn()(), fn()()) >= (1, 1));
  ignore2((1, 1) >= (fn()(), fn()()));
  ignore(fn()() <= fn()());
  ignore2((fn()(), fn()()) <= (1, 1));
  ignore2((1, 1) <= (fn()(), fn()()));
  ignore(fn()() > fn()());
  ignore2((fn()(), fn()()) > (1, 1));
  ignore2((1, 1) > (fn()(), fn()()));
  ignore(fn()() < fn()());
  ignore2((fn()(), fn()()) < (1, 1));
  ignore2((1, 1) < (fn()(), fn()()));

  ignore($&&(fn()(), fn()()));
  ignore($||(fn()(), fn()()));
  ignore($&(fn()(), fn()()));
  ignore($|(fn()(), fn()()));
  ignore($^(fn()(), fn()()));
  ignore($<<(fn()(), fn()()));
  ignore($>>(fn()(), fn()()));
  ignore($**(fn()(), fn()()));
  ignore($%(fn()(), fn()()));
  ignore($+(fn()(), fn()()));
  ignore($-(fn()(), fn()()));
  ignore($*(fn()(), fn()()));
  ignore($/(fn()(), fn()()));
  ignore($==(fn()(), fn()()));
  ignore($!=(fn()(), fn()()));
  ignore($>=(fn()(), fn()()));
  ignore($<=(fn()(), fn()()));
  ignore($>(fn()(), fn()()));
  ignore($<(fn()(), fn()()));

  ignore(!fn()());
  ignore(~fn()());
  ignore(+fn()());
  ignore(-fn()());
  ignoref(fn()().);
  ignore([fn()().]);
  ignore((fn()(), fn()())[fn()()]);
  fn2(fn()(), fn()())(fn()());

  var a = 0;
  a &&= fn()();
  a ||= fn()();
  a &= fn()();
  a |= fn()();
  a ^= fn()();
  a <<= fn()();
  a >>= fn()();
  a **= fn()();
  a %= fn()();
  a += fn()();
  a -= fn()();
  a *= fn()();
  a /= fn()();
  a;

  fn();
  const f = fn();
  var g = fn();
  g = fn();
  f();
  g();
  const ff = fn()();
  var gg = fn()();
  gg = fn()();
  ff;
  gg;

  if (fn()()) {
    const t = fn();
    fn();
    t;
  }
  else {
    const t = fn();
    fn();
    t;
  }
  if (fn()()) {
    const t = fn();
    fn();
    t;
  }
  var b = 0;
  while (fn()()) {
    const t = fn();
    fn();
    t;
    ++b;
    if (b == 2) {
      continue;
    }
    if (b == 3) {
      break;
    }
  }
  b = 0;
  do {
    const t = fn();
    fn();
    t;
    ++b;
    if (b == 2) {
      continue;
    }
    if (b == 3) {
      break;
    }
  } while(fn()());
  b = 0;
  for (fn(); fn()(); fn()) {
    const t = fn();
    fn();
    t;
    ++b;
    if (b == 2) {
      continue;
    }
    if (b == 3) {
      break;
    }
  }
  b = 0;
  for (const t = fn(); fn()(); (const t = fn()()) && t) {
    t;
    const t = fn();
    fn();
    t;
    ++b;
    if (b == 2) {
      continue;
    }
    if (b == 3) {
      break;
    }
  }

  return fn()();
};
)";

TEST_F(YangTest, ExhaustiveRefcount)
{
  if (!filter("exhaustive")) {
    return;
  }

  instance(TestExhaustiveRefCountStr).call<int_t>("exhaustive_refcount");
}

// End namespace yang.
}
