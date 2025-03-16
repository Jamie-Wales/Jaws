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
std::vector<std::shared_ptr<ir::TopLevel>> optimise(std::vector<std::shared_ptr<ir::TopLevel>>& anfs, bool print)
{

    printAnf("<| Optimising ANF |>", anfs, print);
    dce(anfs, print);
    // optimiseConstants(anfs);
    printAnf("<| Post ConstantFold ANF |>", anfs, print);
    return anfs;
}

}
