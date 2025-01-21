#include "ANF.h"
#include <unordered_set>
namespace optimise {

void dce(ir::ANF& anf)
{
}

ir::ANF optimise(ir::ANF& anf)
{

    dce(anf);

    return anf;
}

}
