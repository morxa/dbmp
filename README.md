# DBMP: Database-Driven Macro Planning

DBMP is a PDDL macro planner that creates macro actions based on previous
planning results.

## How to run

1.  Set up Kubernetes with ansible. To do so, you need to add an inventory file
    that describes your cluster setup. You need to have nodes for the roles
    `kubelet`, `kubeadm`, `kube`, and `mongodb`. An minimal setup may look like
    this:

    ```yaml
    [kubelet]
    kubelet1 ansible_user=root
    [kubeadm]
    kubemaster ansible_user=root unprivileged_user=kube
    [kube:children]
    kubeadm
    kubelet
    [mongodb]
    mongo1
    ```

2.  Upload your PDDL domain and problems with `./dbmp/upload_pddl.py`  and run
    Kubernetes jobs to compute initial plans. As an example, to start jobs for
    all problems in `./blocksworld/`, run:

    ```
    $ ./upload_pddl.py -c db.conf --start-job -a \
       -t ../kubernetes/planning-job-macro-gen.yaml \
       --domainfile blocksworld/domain.pddl \
       --planner fast-downward \
       blocksworld/problem*.pddl \
       | kubectl -n dbmp create -f -
    ```

    Check `./dbmp/upload_pddl.py -h` for more information on the usage.
3.  Identify frequent action sequences by calling `get_macros(domain,
    max_length)` in `./identification/count_action_sequences.js`. Example:

    ```
    $ mongo --host $mongo_host -u $user -p $mongo_password macro_planning \
      --eval "load('count_action_sequences.js'); get_macros('blocksworld', 3)"
    ```

3.  Generate macro actions and macro-augmented domains with
    `./dbmp/generator.py`. Example:

    ```
    $ ./generator.py -c db.conf --domain icaps-rcll --from-db -a \
      --best-evaluated 1 --best 10 \
      --evaluate --evaluator weighted_fp_evaluator_80_20 \
      -g -s -m 1 --max-actions 2
    ```

4.  Optionally run benchmarks with `./dbmp/benchmark.py`.
5.  To fetch the augmented domains, fetch it from the `domains` collection,
    e.g.,:

    ```
    db.domains.findOne({name: 'blocksworld', augmented: true})['raw']
    ```
