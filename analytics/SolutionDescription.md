<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Solution Description](#solution-description)
    - [Introduction](#introduction)
    - [Assumptions & Simplifications](#assumptions--simplifications)
        - [Output Timing](#output-timing)
        - [Error Handling](#error-handling)
        - [Case Class Validations](#case-class-validations)
    - [Code Structure](#code-structure)
        - [Model](#model)
        - [Pipes](#pipes)
        - [Other Components](#other-components)
    - [Tests](#tests)
        - [Unit Testing](#unit-testing)
        - [Testing The Whole App](#testing-the-whole-app)

<!-- markdown-toc end -->

# Solution Description #

In here I add all the considerations about the coding chanllenge and the though
process behind it.

## Introduction  ##

For this coding challenge I decided to stick with the libraries already into the
skeleton, even if I never used some of them before. In particual this is the
first time I use `cats.effect`, `fs2` and `circe`. 

There are different pros and cons around this decision:
* This choice slowed me down quite a bit and I decided to keep things simple as
  possible because I was not familiar with the tech stack. For example, I
  decided to work on one single stream from the start to the end, avoiding
  spawning new streams or slitting/merging the one I have even if, in a real
  world scenario, you probably want that to get out the most from concurrency.
* I finally had the chance to play with this technologies. That's a thing I was
  looking for since some time.

## Assumptions & Simplifications ##

In this section I grouped all the choices I took to keep the solution simpler
even if, in a real world scenario, you wish to have them properly sorted.

### Output Timing ###

One way I though about how to handle the time was to spawn a stream at the
beginning of the application using `Stream.awakeEvery` and then let it interact
with the input stream. So everytime it emits, basically the aggregation
operation starts.

Then, I dismiss the option mainly for the reason into the
[Introduction](#introduction) section.

Instead I just went for the `debounce` method, providing a fixed time duration.
I choose that expectially because, when it applies, I don't really care about
the intermediate results, but just the state of the stream when the debounce
happens. So, reading from the docs it fits exactly my case:

> Use-case: if this is a stream of updates about external state, we may want to
> refresh (side-effectful) once every 'd' milliseconds, and every time we refresh
> we only care about the latest update.

Finally, for the document aggregation operation I didn't fix the timing window
exactly to the start/end of the time window (12:00 - 13:00) as showned into the
output example, but I just put into the output row the minimum and maximum time
of the visits involved. This was the simplest way to do it, but it's also easy
to just round the time up or down to achieve the result into the challenge
description.

### Configurations ###

I left the configurations and the skeleton as much similar as the starting one
as possible. This way it would be easier to review and I preferred to focus more
on the solution instead. 

In a real world scenario you will like to have better structured configs and
propably also estract the timing constants to a config file in order to be able
to change them on the fly if needed.

### Error Handling ###

Regarding the error handling I just decided to print into the standard output every
problem I encountered in processing the input stream, in particular:
* Parsing problems
* Unexpected Messages: for example `VisitUpdate` without Previous `VisitCreate`.

Looking at the input stream, turns out that the second case happens a lot. Then
I just decided to comment out the line in order to grasp the actual output
during a run.

Ideally, you wish to split the stream when an error occurs and then handle them
properly and one by one.

### Case Class Validations ###

In the solution I showcase here and there the usage of the smart constructors to
validate the input of a case class. Anyway, I filtered out some of them in order
to keep it clean, instead of having to deal with options so often.

For example, there's no validation of `startDate` and `endDate` during the
creation of the case classes.

### Logging ###

I thought about inserting the logging in the first place (in a functional way),
but that would meant using the `Writer` monad into the streams or having a
separate stream dedicaded for the logging, in order to emit the message in
there.

I just prefered to not add it, but what I want to say is that I had it in my
mind.

## Code Structure ##

A brief description of the coding structure

### Model ###

In this package I collect all the types and data structures, mainly case
classes, I used into the solution. In particular, the `core` inner package
contains types for:
* Visit: Model of the incoming message
* VisitSummary: data used to fold visit by id
* DocumentSummary: data representing the outcome of the application.

Then, into the `json` package I just add the datatype for the input data, in
fact the `Message` trait and case classes reflect exactly what the application
expects as input.

Finally the `core` package has a `package.scala` where I collect all the type
aliases and implicits I needed.

### Pipes ###

Having a single stream solution, all the logic is applied by the pipes. That's
why there's a dedicaded packaeg for that. In detail, I identify three main
steps:
1. **InputParsing:** transform the raw string input into the internal Visit
   representation. In here the validations regaring the input parsing happens.
2. **VisitAggregation:** Takes the outcome of the last step and merge the visits by id, producing a
   stream of `VisitSummary`. The most important steps here are: 
   * The filtering of the current state by the time window
   * The validation of the input by the ordering in come through
   * The error handling, skipped. See [Error Handling](#error-handling)
3. **DocumentAggregation:** From the `VisitSummary` stream it produce the
   `DocumentSummary` output stream.

### Other Components ###

The files left are:
* **CirceGenericDerivation:** Used to instruct `circe` how to parse specific
  types properly.
* **Main:** where all the steps are combined together.

## Tests ##

### Unit Testing ###

Pretty much all the unit testing are crafted using the `scalacheck` library and
generating the inputs automatically.

In particular the file **InputGenerator** collects all the generators used to
produce the input data.

Most of the code is tested against the happy path, but more tests can be
developed to produce invalid input and catch the expected errors.

### Testing The Whole App ###

For testing the Whole app I just extracted some data from the input test
provided and build a small, controlled input where a test could just check the
result from the application with the result of the sample input.

In this case I used `scalatest`. It could be done with `scalacheck` as well but
this time, since I didn't see the needs of generating random input, I just use
the classical `scalatest` library.