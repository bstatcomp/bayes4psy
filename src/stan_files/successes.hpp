/*
    EasyBayes is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    EasyBayes is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with EasyBayes.  If not, see <http://www.gnu.org/licenses/>.
*/
#ifndef MODELS_HPP
#define MODELS_HPP
#define STAN__SERVICES__COMMAND_HPP
#include <rstan/rstaninc.hpp>
// Code generated by Stan version 2.18.0

#include <stan/model/model_header.hpp>

namespace model_successes_namespace {

using std::istream;
using std::string;
using std::stringstream;
using std::vector;
using stan::io::dump;
using stan::math::lgamma;
using stan::model::prob_grad;
using namespace stan::math;

static int current_statement_begin__;

stan::io::program_reader prog_reader__() {
    stan::io::program_reader reader;
    reader.add_event(0, 0, "start", "model_successes");
    reader.add_event(24, 22, "end", "model_successes");
    return reader;
}

#include <meta_header.hpp>
 class model_successes : public prob_grad {
private:
    int n;
    int m;
    vector<int> r;
    vector<int> s;
public:
    model_successes(stan::io::var_context& context__,
        std::ostream* pstream__ = 0)
        : prob_grad(0) {
        ctor_body(context__, 0, pstream__);
    }

    model_successes(stan::io::var_context& context__,
        unsigned int random_seed__,
        std::ostream* pstream__ = 0)
        : prob_grad(0) {
        ctor_body(context__, random_seed__, pstream__);
    }

    void ctor_body(stan::io::var_context& context__,
                   unsigned int random_seed__,
                   std::ostream* pstream__) {
        typedef double local_scalar_t__;

        boost::ecuyer1988 base_rng__ =
          stan::services::util::create_rng(random_seed__, 0);
        (void) base_rng__;  // suppress unused var warning

        current_statement_begin__ = -1;

        static const char* function__ = "model_successes_namespace::model_successes";
        (void) function__;  // dummy to suppress unused var warning
        size_t pos__;
        (void) pos__;  // dummy to suppress unused var warning
        std::vector<int> vals_i__;
        std::vector<double> vals_r__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning

        // initialize member variables
        try {
            current_statement_begin__ = 2;
            context__.validate_dims("data initialization", "n", "int", context__.to_vec());
            n = int(0);
            vals_i__ = context__.vals_i("n");
            pos__ = 0;
            n = vals_i__[pos__++];
            current_statement_begin__ = 3;
            context__.validate_dims("data initialization", "m", "int", context__.to_vec());
            m = int(0);
            vals_i__ = context__.vals_i("m");
            pos__ = 0;
            m = vals_i__[pos__++];
            current_statement_begin__ = 4;
            validate_non_negative_index("r", "n", n);
            context__.validate_dims("data initialization", "r", "int", context__.to_vec(n));
            validate_non_negative_index("r", "n", n);
            r = std::vector<int>(n,int(0));
            vals_i__ = context__.vals_i("r");
            pos__ = 0;
            size_t r_limit_0__ = n;
            for (size_t i_0__ = 0; i_0__ < r_limit_0__; ++i_0__) {
                r[i_0__] = vals_i__[pos__++];
            }
            current_statement_begin__ = 5;
            validate_non_negative_index("s", "n", n);
            context__.validate_dims("data initialization", "s", "int", context__.to_vec(n));
            validate_non_negative_index("s", "n", n);
            s = std::vector<int>(n,int(0));
            vals_i__ = context__.vals_i("s");
            pos__ = 0;
            size_t s_limit_0__ = n;
            for (size_t i_0__ = 0; i_0__ < s_limit_0__; ++i_0__) {
                s[i_0__] = vals_i__[pos__++];
            }

            // validate, data variables
            current_statement_begin__ = 2;
            check_greater_or_equal(function__,"n",n,0);
            current_statement_begin__ = 3;
            check_greater_or_equal(function__,"m",m,0);
            current_statement_begin__ = 4;
            for (int k0__ = 0; k0__ < n; ++k0__) {
                check_greater_or_equal(function__,"r[k0__]",r[k0__],0);
                check_less_or_equal(function__,"r[k0__]",r[k0__],1);
            }
            current_statement_begin__ = 5;
            for (int k0__ = 0; k0__ < n; ++k0__) {
                check_greater_or_equal(function__,"s[k0__]",s[k0__],0);
            }
            // initialize data variables


            // validate transformed data

            // validate, set parameter ranges
            num_params_r__ = 0U;
            param_ranges_i__.clear();
            current_statement_begin__ = 10;
            validate_non_negative_index("p", "m", m);
            num_params_r__ += m;
            current_statement_begin__ = 11;
            ++num_params_r__;
            current_statement_begin__ = 12;
            ++num_params_r__;
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
    }

    ~model_successes() { }


    void transform_inits(const stan::io::var_context& context__,
                         std::vector<int>& params_i__,
                         std::vector<double>& params_r__,
                         std::ostream* pstream__) const {
        stan::io::writer<double> writer__(params_r__,params_i__);
        size_t pos__;
        (void) pos__; // dummy call to supress warning
        std::vector<double> vals_r__;
        std::vector<int> vals_i__;

        if (!(context__.contains_r("p")))
            throw std::runtime_error("variable p missing");
        vals_r__ = context__.vals_r("p");
        pos__ = 0U;
        validate_non_negative_index("p", "m", m);
        context__.validate_dims("initialization", "p", "vector_d", context__.to_vec(m));
        vector_d p(static_cast<Eigen::VectorXd::Index>(m));
        for (int j1__ = 0U; j1__ < m; ++j1__)
            p(j1__) = vals_r__[pos__++];
        try {
            writer__.vector_lub_unconstrain(0,1,p);
        } catch (const std::exception& e) { 
            throw std::runtime_error(std::string("Error transforming variable p: ") + e.what());
        }

        if (!(context__.contains_r("a")))
            throw std::runtime_error("variable a missing");
        vals_r__ = context__.vals_r("a");
        pos__ = 0U;
        context__.validate_dims("initialization", "a", "double", context__.to_vec());
        double a(0);
        a = vals_r__[pos__++];
        try {
            writer__.scalar_lb_unconstrain(0,a);
        } catch (const std::exception& e) { 
            throw std::runtime_error(std::string("Error transforming variable a: ") + e.what());
        }

        if (!(context__.contains_r("b")))
            throw std::runtime_error("variable b missing");
        vals_r__ = context__.vals_r("b");
        pos__ = 0U;
        context__.validate_dims("initialization", "b", "double", context__.to_vec());
        double b(0);
        b = vals_r__[pos__++];
        try {
            writer__.scalar_lb_unconstrain(0,b);
        } catch (const std::exception& e) { 
            throw std::runtime_error(std::string("Error transforming variable b: ") + e.what());
        }

        params_r__ = writer__.data_r();
        params_i__ = writer__.data_i();
    }

    void transform_inits(const stan::io::var_context& context,
                         Eigen::Matrix<double,Eigen::Dynamic,1>& params_r,
                         std::ostream* pstream__) const {
      std::vector<double> params_r_vec;
      std::vector<int> params_i_vec;
      transform_inits(context, params_i_vec, params_r_vec, pstream__);
      params_r.resize(params_r_vec.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r(i) = params_r_vec[i];
    }


    template <bool propto__, bool jacobian__, typename T__>
    T__ log_prob(vector<T__>& params_r__,
                 vector<int>& params_i__,
                 std::ostream* pstream__ = 0) const {

        typedef T__ local_scalar_t__;

        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning

        T__ lp__(0.0);
        stan::math::accumulator<T__> lp_accum__;

        try {
            // model parameters
            stan::io::reader<local_scalar_t__> in__(params_r__,params_i__);

            Eigen::Matrix<local_scalar_t__,Eigen::Dynamic,1>  p;
            (void) p;  // dummy to suppress unused var warning
            if (jacobian__)
                p = in__.vector_lub_constrain(0,1,m,lp__);
            else
                p = in__.vector_lub_constrain(0,1,m);

            local_scalar_t__ a;
            (void) a;  // dummy to suppress unused var warning
            if (jacobian__)
                a = in__.scalar_lb_constrain(0,lp__);
            else
                a = in__.scalar_lb_constrain(0);

            local_scalar_t__ b;
            (void) b;  // dummy to suppress unused var warning
            if (jacobian__)
                b = in__.scalar_lb_constrain(0,lp__);
            else
                b = in__.scalar_lb_constrain(0);


            // transformed parameters



            // validate transformed parameters

            const char* function__ = "validate transformed params";
            (void) function__;  // dummy to suppress unused var warning

            // model body

            current_statement_begin__ = 16;
            lp_accum__.add(beta_log<propto__>(p, a, b));
            current_statement_begin__ = 19;
            for (int i = 1; i <= n; ++i) {

                current_statement_begin__ = 20;
                lp_accum__.add(bernoulli_log<propto__>(get_base1(r,i,"r",1), get_base1(p,get_base1(s,i,"s",1),"p",1)));
            }

        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }

        lp_accum__.add(lp__);
        return lp_accum__.sum();

    } // log_prob()

    template <bool propto, bool jacobian, typename T_>
    T_ log_prob(Eigen::Matrix<T_,Eigen::Dynamic,1>& params_r,
               std::ostream* pstream = 0) const {
      std::vector<T_> vec_params_r;
      vec_params_r.reserve(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        vec_params_r.push_back(params_r(i));
      std::vector<int> vec_params_i;
      return log_prob<propto,jacobian,T_>(vec_params_r, vec_params_i, pstream);
    }


    void get_param_names(std::vector<std::string>& names__) const {
        names__.resize(0);
        names__.push_back("p");
        names__.push_back("a");
        names__.push_back("b");
    }


    void get_dims(std::vector<std::vector<size_t> >& dimss__) const {
        dimss__.resize(0);
        std::vector<size_t> dims__;
        dims__.resize(0);
        dims__.push_back(m);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dimss__.push_back(dims__);
    }

    template <typename RNG>
    void write_array(RNG& base_rng__,
                     std::vector<double>& params_r__,
                     std::vector<int>& params_i__,
                     std::vector<double>& vars__,
                     bool include_tparams__ = true,
                     bool include_gqs__ = true,
                     std::ostream* pstream__ = 0) const {
        typedef double local_scalar_t__;

        vars__.resize(0);
        stan::io::reader<local_scalar_t__> in__(params_r__,params_i__);
        static const char* function__ = "model_successes_namespace::write_array";
        (void) function__;  // dummy to suppress unused var warning
        // read-transform, write parameters
        vector_d p = in__.vector_lub_constrain(0,1,m);
        double a = in__.scalar_lb_constrain(0);
        double b = in__.scalar_lb_constrain(0);
            for (int k_0__ = 0; k_0__ < m; ++k_0__) {
            vars__.push_back(p[k_0__]);
            }
        vars__.push_back(a);
        vars__.push_back(b);

        // declare and define transformed parameters
        double lp__ = 0.0;
        (void) lp__;  // dummy to suppress unused var warning
        stan::math::accumulator<double> lp_accum__;

        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning

        try {



            // validate transformed parameters

            // write transformed parameters
            if (include_tparams__) {
            }
            if (!include_gqs__) return;
            // declare and define generated quantities



            // validate generated quantities

            // write generated quantities
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
    }

    template <typename RNG>
    void write_array(RNG& base_rng,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& params_r,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& vars,
                     bool include_tparams = true,
                     bool include_gqs = true,
                     std::ostream* pstream = 0) const {
      std::vector<double> params_r_vec(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r_vec[i] = params_r(i);
      std::vector<double> vars_vec;
      std::vector<int> params_i_vec;
      write_array(base_rng,params_r_vec,params_i_vec,vars_vec,include_tparams,include_gqs,pstream);
      vars.resize(vars_vec.size());
      for (int i = 0; i < vars.size(); ++i)
        vars(i) = vars_vec[i];
    }

    static std::string model_name() {
        return "model_successes";
    }


    void constrained_param_names(std::vector<std::string>& param_names__,
                                 bool include_tparams__ = true,
                                 bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        for (int k_0__ = 1; k_0__ <= m; ++k_0__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "p" << '.' << k_0__;
            param_names__.push_back(param_name_stream__.str());
        }
        param_name_stream__.str(std::string());
        param_name_stream__ << "a";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "b";
        param_names__.push_back(param_name_stream__.str());

        if (!include_gqs__ && !include_tparams__) return;

        if (include_tparams__) {
        }


        if (!include_gqs__) return;
    }


    void unconstrained_param_names(std::vector<std::string>& param_names__,
                                   bool include_tparams__ = true,
                                   bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        for (int k_0__ = 1; k_0__ <= m; ++k_0__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "p" << '.' << k_0__;
            param_names__.push_back(param_name_stream__.str());
        }
        param_name_stream__.str(std::string());
        param_name_stream__ << "a";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "b";
        param_names__.push_back(param_name_stream__.str());

        if (!include_gqs__ && !include_tparams__) return;

        if (include_tparams__) {
        }


        if (!include_gqs__) return;
    }

}; // model

}

typedef model_successes_namespace::model_successes stan_model;


#endif
