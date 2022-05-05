//# thylacine<img align="right" src="https://github.com/gvonnes/thylacine/blob/main/docs/assets/logo.svg?raw=true" height="200px" style="padding-left: 20px"/>

# Thylacine
![Build Status](https://github.com/gvonness/thylacine/actions/workflows/build.yml/badge.svg)

First, let me say this work is very much a WIP; please be patient :).

Thylacine is a Bayesian inference framework that facilitates sampling, integration and subsequent statistical analysis on posterior distributions born out of a Bayesian inference.

A few aspects differentiate Thylacine from other Bayesian inference frameworks:
* Framework and design largely de-coupled from Bayesian graphical representations of the problem (more details in the FAQ below)
* Designed to be multi-threaded from the ground up using a high-performance Software Transactional Memory (STM) implementation to facilitate efficient concurrency across the differing computational profiles for sampling, integration, visualisation, etc.
* Analytic gradient calculations can be specified on a component level that enables automatic differentiation to be used in gradient calculations
* A growing list of advanced algorithms have already been implementated:
  * Hamiltonian MCMC
  * An advanced version of Nested Sampling: Stochastic Lebesgue Quadrature (SLQ) 
  * Gaussian Analytic Posteriors

---

## Theory

The theory of Bayesian inference is too vast to cover here. However, two references I highly recommend are:
* [Information Theory, Inference, and Learning Algorithms](https://www.inference.org.uk/itprnn/book.pdf) - An amazing book that shows how deeply connected Bayesian inference, coding theory, information theory, ML models and learning really are (i.e. all different facets of the same concept). What's even better is that David MacKay has made the book freely available to the public (definitely worth buying if you like physical books though).
* [Data Analysis: A Bayesian Tutorial](https://blackwells.co.uk/bookshop/product/Data-Analysis-by-D-S-Sivia-J-Skilling/9780198568322) - While many references can get bogged down in theory that can be cumbersome to penetrate for pragmatic approaches, this book cuts right to the chase and focuses on experimental data analysis from the start. This is an excellent reference for people who want to get up and running fast with Bayesian methods. The book is also a much faster read at ~250 pages than most other references out there.

---

## Framework Documentation

Coming Soon!

---
---

## PR FAQ
### What is a PR FAQ?
Take a look at [the Medium article on PR FAQs](https://medium.com/agileinsider/press-releases-for-product-managers-everything-you-need-to-know-942485961e31) for a good overview of the concept. I have taken some liberties with the formatting, but I generally like the concept of a living FAQ to help introduce products.

### Why another Bayesian framework?
I got involved with Bayesian techniques a long time ago when I was working as a plasma physicist. In that time most of the Bayesian data analysis I did was via a bespoke framework written in Java. Over the course of my research, I saw many avenues of improvement for this framework that mostly centred around data modelling and performance optimisations. However, the nature of research did not afford me to the time to work on tooling improvement over getting research results. 

After I left academia, I wanted to implement these improvements but lacked a good problem to test my ideas against. Also, extending the framework I was using wasn't really feasible, as the project was closed sourced with commercial aspirations. Eventually, I came across an interesting problem of inferring mass density distributions of Katana when I started training in Battojutsu. As I continued to flesh out details in the forward model for this problem, the supporting inference code became more complex; and it became clear it was time to extract out the underpinning framework from the application code. Hence, Thylacine was born.

### Will you add tests?
Yes, the first step was to simply extract out the inference framework from the BayKen application code (in another repo now). I have been doing extensive testing in the context of higher level inference validation using this codebase. 

How to meaningfully test a framework like this I find to be a very interesting problem in and of itself. I am currently working on a simple multi-variate normal distribution analytic test case that will serve as the centerpiece for a top-down testing strategy.

### Why 'Thylacine'?
Tasmanian tigers have been one of my favourite animals since I was a kid. I really hope we can bring them back someday. I named this framework after then, as this framework is about meaningfully merging data from a heterogenous collection of measurements, and I see thylacines as a bit of a chimera with respect to other animals: they are marsupials, carnivorous, have stripes, and exhibit both feline and canine qualities. I.e. both are about combining different parts to get a better whole. If that's too much of a stretch, then let's just say it's artistic license :).