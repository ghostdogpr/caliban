package caliban.tools

import zio.config.magnolia.Descriptor
import zio.config.{ ConfigDescriptor, ConfigSource }

object DescriptorUtils {
  def from[A](configSource: ConfigSource)(implicit config: Descriptor[A]): ConfigDescriptor[A] =
    Descriptor[A].from(configSource)
  }
}
