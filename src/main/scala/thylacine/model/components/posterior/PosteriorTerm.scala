package ai.entrolution
package thylacine.model.components.posterior

import thylacine.model.core.GenericIdentifier.TermIdentifier

private[thylacine] trait PosteriorTerm {
  private[thylacine] def posteriorTermIdentifier: TermIdentifier
}
