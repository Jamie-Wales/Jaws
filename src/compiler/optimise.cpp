#include "ANF.h"
#include "ConstantFold.h"
#include "DeadCodeElimination.h"
#include <iostream>

namespace optimise {

void printAnf(std::string message, std::vector<std::shared_ptr<ir::ANF>>& anfs, bool print)
{
    std::cout << message << std::endl;
    std::cout << "--------------------" << std::endl;
    if (print) {
        for (const auto& anf : anfs)
            std::cout << anf->toString() << "\n";
    }
    std::cout << std::endl;
}

std::vector<std::shared_ptr<ir::ANF>> optimise(std::vector<std::shared_ptr<ir::ANF>>& anfs, bool print)
{

    printAnf("Optimising ANF:", anfs, print);
    // optimiseConstants(anfs);
    // printAnf("Constant Folded ANF:", anfs, print);
    elimatedDeadCode(anfs);
    printAnf("DCE ANF:", anfs, print);

    return anfs;
}

}
