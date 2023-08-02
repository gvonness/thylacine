# Thylacine

![Build Status](https://github.com/gvonness/thylacine/actions/workflows/build.yml/badge.svg)
[![Maven Central](https://img.shields.io/maven-central/v/ai.entrolution/thylacine_2.13)](https://maven-badges.herokuapp.com/maven-central/ai.entrolution/thylacine_2.13)

First, let me say this work is very much a WIP; please be patient :).

Thylacine is a FP (Functional Programming) Bayesian inference framework that facilitates sampling, integration and
subsequent statistical analysis on posterior distributions born out of a Bayesian inference.

A few aspects differentiate Thylacine from other Bayesian inference frameworks:

* Fully FP - Designed from the ground-up to fully leverage the virtues of FP in internal computations, while providing
  generic FP API.
* Framework and design largely de-coupled from Bayesian graphical representations of the problem (more details in the
  FAQ below)
* Designed to be multi-threaded from the ground up using a high-performance Software Transactional Memory (STM)
  implementation to facilitate efficient concurrency across the differing computational profiles for sampling,
  integration, visualisation, etc.
* Analytic gradient calculations can be specified on a component level that enables automatic differentiation to be used
  in gradient calculations. This is essential if aiming to perform high-dimension inference using gradient information.
* A growing list of advanced algorithms/concepts have already been implemented:
    * Gaussian analytic posteriors
    * Optimisation
        * Hooke & Jeeves
        * Coordinate line search (Coordinate Slide)
        * Multi-direction search (MDS)
        * Nonlinear conjugate gradient
    * Sampling
        * Hamiltonian MCMC
        * Leapfrog MCMC
    * Integration/Sampling
        * An advanced version of Nested Sampling: Stochastic Lebesgue Quadrature (SLQ)
    * General
        * Component-wise automatic differentiation that falls back to finite-differences when analytic gradient
          calculations are not available for a particular component
        * Likelihoods that support in-memory caching of evaluation and Jacobian calculations
        * Support for Cauchy distributions (in addition to standard Gaussian and uniform distributions)

---

## Theory

The theory of Bayesian inference is too vast to cover here. However, two references I highly recommend are:

* [Information Theory, Inference, and Learning Algorithms](https://www.inference.org.uk/itprnn/book.pdf) - An amazing
  book that shows how deeply connected Bayesian inference, coding theory, information theory, ML models and learning
  really are (i.e. all different facets of the same concept). What's even better is that David MacKay has made the book
  freely available to the public (definitely worth buying if you like physical books though).
* [Data Analysis: A Bayesian Tutorial](https://blackwells.co.uk/bookshop/product/Data-Analysis-by-D-S-Sivia-J-Skilling/9780198568322) -
  While many references can get bogged down in theory that can be cumbersome to penetrate for pragmatic approaches, this
  book cuts right to the chase and focuses on experimental data analysis from the start. This is an excellent reference
  for people who want to get up and running fast with Bayesian methods. The book is also a much faster read at ~250
  pages than most other references out there.

---

## Quick Start

To use Thylacine in an existing SBT project with Scala 2.13 or a later version, add the following dependency to your
`build.sbt`:

```scala
libraryDependencies += "ai.entrolution" %% "thylacine" % VERSION
```

See the Maven badge above for the latest version.

---

## Framework Documentation

Coming Soon!

---
---

## PR FAQ

### What is a PR FAQ?

Take a look
at [the Medium article on PR FAQs](https://medium.com/agileinsider/press-releases-for-product-managers-everything-you-need-to-know-942485961e31)
for a good overview of the concept. I have taken some liberties with the formatting, but I generally like the concept of
a living FAQ to help introduce products.

### Why another Bayesian framework?

I became involved with Bayesian techniques a long time ago when I was working as a plasma physicist. In that time most
of the Bayesian data analysis I did was via a bespoke framework written in Java. Over the course of my research, I saw
many avenues of improvement for this framework that mostly centred around data modelling and performance optimisations.
However, the nature of research did not afford me to the time to work on tooling improvement over getting research
results.

After I left academia, I wanted to implement these improvements but lacked a good problem to test my ideas against.
Also, extending the framework I was using wasn't really feasible, as the project was closed sourced with commercial
aspirations. Eventually, I came across an interesting problem of inferring mass density distributions of Katana when I
started training in Battojutsu. As I continued to flesh out details in the forward model for this problem, the
supporting inference code became more complex; and it became clear it was time to extract out the underpinning framework
from the application code. Hence, Thylacine was born.

### What about Graphical models?

The framework is decoupled from any concepts in Bayesian graphical models by design. Indeed, I found previously that
trying to integrate graphical concepts into a low-level Bayesian framework led to unneeded coupling within the data
model, when just needing to perform posterior analysis. Indeed, priors and likelihoods can be formulated in any desired
context and then fed into this framework.

More abstractly, the concept of probabalistic graphical models is not intrinsically linked to Bayesian analysis. Indeed,
frequentist methods can also be used to process these graphs representations. Given this, I decided it did not make
sense to artificially couple two concepts within this framework that are not intrinsically linked together (for
more-or-less standard software engineering reasons not to introduce unneeded coupling in one's code).

### Will you add tests?

How to meaningfully test a framework like this I find to be a very interesting problem in and of itself. I have some
simple tests that cover simple analytic and non-analytic inferences. However, figuring out how to meaningful test the
sampling inside a unit test in a deterministic and fast way is something I'm still working on.

The first step was to simply extract out the inference framework from the BayKen application code (in another repo now).
I have been doing extensive testing in the context of higher level inference validation using this codebase.

### Why 'Thylacine'?

Tasmanian tigers have been one of my favourite animals since I was a kid. I really hope we can bring them back someday.
I named this framework after then, as this framework is about meaningfully merging data from a heterogenous collection
of measurements, and I see thylacines as a bit of a chimera with respect to other animals: they are marsupials,
carnivorous, have stripes, and exhibit both feline and canine qualities. I.e. both are about combining different parts
to get a better whole. If that's too much of a stretch, then let's just say it's artistic license :).