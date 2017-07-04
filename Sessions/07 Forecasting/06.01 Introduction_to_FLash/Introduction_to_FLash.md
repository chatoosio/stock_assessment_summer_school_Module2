% Forecasting with FLR - an introduction to FLash
% Finlay Scott & Iago Mosqueira
% August 2015

# FLash!

\includegraphics[keepaspectratio, height = 2in, width=\textwidth]{graphics/flash_gordon.jpg}

\centerline{Ahhhhhhhh!}

# FLash!

\includegraphics[keepaspectratio, height = 2in, width=\textwidth]{graphics/scream.jpg}

\centerline{Aggggghhh!}

# Performing projections

Why?

* Predict the future
* Understand potential consequences of management actions

Results depend on many, many assumptions


# Performing projections

Things to consider:

* Time scale - 3 years? 5? 20?
* Stock biology - growth and recruitment?
* Incorporate uncertainty
* Model the management scenario

Again: Results depend on many, many assumptions

# But remember

*It is difficult to make predictions, especially about the future.*

<http://quoteinvestigator.com/2013/10/20/no-predict/>

*A good forecaster is not smarter than everyone else, he merely has his ignorance better organised.*

<http://www1.secam.ex.ac.uk/famous-forecasting-quotes.dhtml>

# The importance of F

Management can have many aims.  
To achieve these aims it sets targets:

* Effort
* Catch (Landings or Discards)
* Biomass (SSB)
* Economic (Revenue)

To hit these targets all we can really control is the fishing mortality, F

# Introducing `fwd()`

`fwd()` is the engine of projection in FLR  
Can be easy to set up - just need to combine the right components
Very easy to get wrong  
Success tastes great!

# Cake

\includegraphics[keepaspectratio, height = 1in, width=\textwidth]{graphics/cake_ingredients.jpeg}

Making a cake requires combining several ingredients  
Some cakes are more difficult to make than others  
Can go badly wrong  
Success tastes great!  
`fwd()` = cake

# Basic ingredients

* Stock to be projected
* Stock-recruitment relationship
* Projection control (set targets)

# The stock

Obviously, this means an `FLStock`

* Need to set up the stock 'future'
* Requires assumptions to be made, e.g. future stock weights, natural mortality etc.
* `stf()`

A simple example...

# The control object

* `fwdControl`
* Set target type and value for each year of the projection
* Can have multiple targets
* Can include bounds on the targets (min and max)
* Target values can be relative
* Target values can have multiple iterations (stochasticity)
* Can be complicated...

But it does not have to be complicated.  
Sometimes, the simplest things are the tastiest...

# Recipe 1 - Flapjack

\includegraphics[keepaspectratio, height = 1in, width=\textwidth]{graphics/flapjack.jpg}

\centerline{Setting F as a target}

# Recipe 2 - Welsh cake

\includegraphics[keepaspectratio, height=1in, width=\textwidth]{graphics/welsh_cake.jpeg}

\centerline{Setting Catch as a target}

Other target types include:  
F, SSB, Landings, Discards, Z, Effort, Costs, Revenue...

# Recipe 3 - Muffin (blueberry)

\includegraphics[keepaspectratio, height = 1in, width=\textwidth]{graphics/muffin.jpeg}

\centerline{SSB as a target}  
Be careful with timing:  
The target year is the year that F is set  
SSB in year Y is a result of F in year Y-1

# Recipe 4 - Cupcake

\includegraphics[keepaspectratio, height=1in, width=\textwidth]{graphics/cupcake.jpeg}

\centerline{Setting catch as a RELATIVE target}

# Recipe 5 - Clootie dumpling

\includegraphics[keepaspectratio, height = 1in, width=\textwidth]{graphics/clootie.jpeg}  
http://en.wikipedia.org/wiki/Clootie  

\centerline{Multiple targets, one with bounds}  

The order of targets in the control object is important

# Recipe 6 - Panettone

\includegraphics[keepaspectratio, height = 1in, width=\textwidth]{graphics/panettone.jpeg}

\centerline{Combining bounds AND relative targets}

# A moment of reflection

So far we have looked at projections with:

* absolute target values,
* relative target values,
* bounds on targets, and
* mixed target types.

But all of the projections have been deterministic  
No consideration of uncertainty  
Prepare yourself!  
Because we are about to enter the 6th Dimension...

(mangiamo)

# Projecting with uncertainty

`fwd()` is happy to work over iterations  
Each iteration is treated separately  
There are several main ways of introducing iterations into fwd():

* Residuals to the stock-recruitment function 
* Through the control object (set targets with multiple values)
* The stock object you have set up (e.g. stochastic weights-at-age)

You can actually use all of these methods at the same time  
As you can probably imagine, this can quickly become very complicated

Before cooking, we need to some preparation with the stock object...

# Using stock-recruitment residuals

\centerline{\includegraphics[keepaspectratio, height = 2.5in, width=\textwidth]{graphics/srr.png}}

# Using stock-recruitment residuals

Two arguments to `fwd()` we have not used yet

* sr.residuals - `FLQuant` of residuals
* sr.residuals.mult - The residuals are multiplicative or additive

We need to make an `FLQuant` of residuals...

# Recipe 7 - Rum and ginger cake

\includegraphics[keepaspectratio, height = 1in, width=\textwidth]{graphics/rum_and_ginger.jpg}

Introducing stock recruitment residuals

# Recipe 8 - Wedding cake

\includegraphics[keepaspectratio, height = 1in, width=\textwidth]{graphics/wedding_cake.jpg}

Stochastic targets

# Recipe 9 - Just a huge cake

\includegraphics[keepaspectratio, height = 2in, width=\textwidth]{graphics/big_ass_cake.jpg}

Everything all at once...


