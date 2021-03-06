# Interneuron-interneuron synaptogenesis does not preclude seizure suppression by generalized inhibitory synaptogenesis in the hippocampus
*A micro-manuscript by Daniel Acker*

**Introduction**

In a recent research manuscript, I describe a novel epilepsy therapy in which seizures are treated by driving the formation of new inhibitory synapses in the hippocampus (Acker et al., 2018). One potential complication of this theraputic approach is that new inhibitory synapses could be formed onto inhibitory neurons, thus suppressing inhibitory activity. Here, I use a computational model to address this concern.

**Results and Discussion**

To address the question of whether increased interneuron inhibition could be counterproductive, I turned to the classic Wendling-Chauvel mean field model of hippocampal seizure generation (Wendling et al., 2002). This model consists of a central excitatory cell population with an excitatory feedback loop and two inhibitory feedback loops, one with fast inhibitory post-synaptic currents (IPSCs) and one with slow IPSCs. The slow inhibitory population innervates both the excitatory population and the fast, inhibitory population. The main output of the model is simulated local field potential (LFP) resulting from the sum of post-synaptic potentials in the central excitatory population. We manipulated synapse number between various cell types in this model and found that the increased inhibition resulting from increased interneuron-to-excitatory-neuron synapse number was sufficient to abolish or attenuate seizure-like activity in the simulated LFP (Fig. 1). This effect was typically not reversed by a 30% increase in interneuron-to-interneuron synapse number. In addition, our *in vivo* seizure experiments (Acker et al., 2018) demonstrate that Sema4D treated mice were resistant to seizure induction, providing empirical evidence that any disinhibition does not overshadow the effect of an overall increase in inhibition.

<img src="https://github.com/AckerDWM/Inhibitory-synaptogenesis-mean-field-EEG/blob/master/figure-with-error-bars.png" alt="alt text" width="500" height="500">

**Figure 1. Effect of increased GABAergic synapse density on simulated LFP in the Wendling-Chauvel model.** LFP was simulated in the Wendling-Chauvel model across a wide range of conditions. (**A**) Log de-trended LFP power under all simulated conditions. Synapse number alterations were baseline (Control), a 30% increase in IE synapses (IE), and a 30% increase in both IE and II synapses (II). The variable A is excitatory synaptic gain, B is slow inhibitory synaptic gain, and G is fast inhibitory synaptic gain. (**B**) Log de-trended LFP power versus synapse number alteration and excitatory synaptic gain. Green asterisks indicate a decrease compared to control at the corresponding level of excitatory synaptic gain; red asterisks indicate an increase. ***p<0.001, **p<0.01, two-tailed Wilcoxon tests with Bonferroni corrections. (**C**) Simulated LFP demonstrating seizure abolition by increased GABAergic synapse number. This effect was not reversed in the II condition. Simulation parameters: A=5, B=35, G=28.

**Methods**

I implemented the Wendling-Chauvel seizure generation model with a step size of five milliseconds. Model parameters were taken from the table 1 of the original paper. For the somatic average time constant 1/g, g was set to 350 sec<sup>-1</sup>. For my simulations, I modified the synapse number parameters C4, C6, and C7. Note that in table 1 of the original paper C6 is said to represent synapses in the fast inhibitory loop, while C7 is said to represent II synapses. This appears to be a typo. Inspection of the model equations and the diagram in figure 1b reveals that C6 represents II synapses, while C7 represents fast IE synapses. My simulations were run for 1200 steps, resulting in 6 seconds of simulated LFP, of which the final 5 seconds were used for further analysis. Independent simulations were run at all levels of a parameter grid comprising all combinations of excitatory synaptic gain (A), slow inhibitory synaptic gain (B), and fast inhibitory synaptic gain (G), over the ranges (3, 7), (1, 50), and (1, 30) respectively in increments of one. This parameter space is similar to that originally explored by Wendling et al. (2002).

**References**

Acker D, Wong I, Kang M, Paradis S (2018) Semaphorin 4D promotes inhibitory synapse formation and suppresses seizures in vivo. Epilepsia 59(6):1257-1268.

Wendling F, Bartolomei F, Bellanger JJ, Chauvel P (2002) Epileptic fast activity can be explained by a model of impaired GABAergic dendritic inhibition. Eur J Neurosci 15(9):1499-508.
