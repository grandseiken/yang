#include "type_info.h"
#include "pipeline.h"

namespace yang {
namespace internal {

const std::string& get_instance_name(const Instance& instance)
{
  return instance.get_program().get_name();
}

const Program& get_instance_program(const Instance& instance)
{
  return instance.get_program();
}

// End namespace yang::internal.
}
}
