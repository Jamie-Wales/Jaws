#include "optimise.h"
#include "ANF.h"
#include "ConstantFold.h"
#include "DeadCodeElimination.h"
#include <iostream>

namespace optimise {
void printAnf(std::string message, std::vector<std::shared_ptr<ir::TopLevel>>& anfs, bool print)
{
    if (print) {
        std::cout << message << std::endl;
        for (const auto& tl : anfs)
            std::cout << tl->toString() << std::endl;
    }
}
OptimizationResult optimise(std::vector<std::shared_ptr<ir::TopLevel>>& anfs)
{
    auto [optimizedAnf, graphs] = dce(anfs);
    optimizedAnf = optimiseConstants(optimizedAnf);
    return {
        optimizedAnf,
        graphs.first,
        graphs.second
    };
}

}
