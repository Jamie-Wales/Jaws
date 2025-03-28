#include "optimise.h"
#include "ANF.h"
#include "DeadCodeElimination.h"

namespace optimise {

OptimizationResult optimise(std::vector<std::shared_ptr<ir::TopLevel>>& anfs)
{
    auto [optimizedAnf, graphs] = dce(anfs);
    
    return {
        optimizedAnf,
        graphs.first,  // pre-optimization graph
        graphs.second  // post-optimization graph
    };
}

}
