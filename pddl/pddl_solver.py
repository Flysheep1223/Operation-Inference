import sys
import argparse
import tempfile
import os
import re

# 导入Pyperplan核心组件（2.x版本稳定API）

from pyperplan.pddl.parser import Parser
from pyperplan.search.a_star import astar_search
from pyperplan.heuristics.relaxation import hFFHeuristic
from pyperplan.grounding import ground


def extract_objects_from_problem(problem_file):
    """从problem文件的init和goal中提取所有位置对象"""
    with open(problem_file, 'r') as f:
        content = f.read()
    
    objects = set()
    
    # 从init部分提取
    init_match = re.search(r'\(:init\s*(.*?)\)\s*\(:goal', content, re.DOTALL)
    if init_match:
        objects.update(re.findall(r'\b(c_\d+_\d+)\b', init_match.group(1)))
    
    # 从goal部分提取
    goal_match = re.search(r'\(:goal\s*(.*?)\)\s*\)\s*$', content, re.DOTALL)
    if goal_match:
        objects.update(re.findall(r'\b(c_\d+_\d+)\b', goal_match.group(1)))
    
    return sorted(list(objects))

def parse_and_ground(domain_file, problem_file):
    """
    解析PDDL文件并ground问题
    自动处理空对象声明问题
    """
    # 读取problem文件
    with open(problem_file, 'r') as f:
        problem_content = f.read()
    
    # 处理空对象声明（你的problem.pddl中objects为空）
    objects_empty = re.search(r'\(:objects\s*\)', problem_content)
    temp_problem_file = None
    
    if objects_empty:
        object_names = extract_objects_from_problem(problem_file)
        if not object_names:
            raise ValueError("No objects found in problem file!")
        
        objects_decl = ' '.join(object_names) + ' - location'
        modified_content = re.sub(
            r'\(:objects\s*\)',
            f'(:objects\n    {objects_decl}\n  )',
            problem_content
        )
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.pddl', delete=False) as f:
            f.write(modified_content)
            temp_problem_file = f.name
        
        problem_path = temp_problem_file
    else:
        problem_path = problem_file
    
    try:
        # 使用Parser解析（最稳定的API）
        parser = Parser(domain_file, problem_path)
        domain = parser.parse_domain()
        problem = parser.parse_problem(domain)
        
        # Ground问题（实例化所有动作）
        task = ground(problem)
        
        # 清理临时文件
        if temp_problem_file and os.path.exists(temp_problem_file):
            os.unlink(temp_problem_file)
            
    except Exception as e:
        if temp_problem_file and os.path.exists(temp_problem_file):
            os.unlink(temp_problem_file)
        raise e
    
    return task

def solve_with_astar(task):
    """
    使用Pyperplan内置A* + hFF启发式求解
    hFF是FF规划器的快速前向启发式，对网格导航问题非常有效
    """
    heuristic = hFFHeuristic(task)
    plan = astar_search(task, heuristic)
    return plan

def main():
    parser = argparse.ArgumentParser(
        description='PDDL grid world solver using pyperplan A*',
        epilog="Example: python pddl_solver.py domain.pddl problem.pddl"
    )
    parser.add_argument('domain', help='PDDL domain file')
    parser.add_argument('problem', help='PDDL problem file')
    
    args = parser.parse_args()
    
    try:
        # 1. 解析并ground问题
        task = parse_and_ground(args.domain, args.problem)
        
        # 2. 使用A*求解（内置算法+启发式）
        plan = solve_with_astar(task)
        
        # 3. 输出结果（与原脚本格式兼容）
        if plan:
            # 提取第一个动作
            first_action = plan[0]
            # pyperplan 2.1 Operator.name 已经是格式化的字符串，如 "(move c_0_0 c_0_1)"
            print(first_action.name)
        else:
            print("; No plan found")
            
    except Exception as e:
        print(f"; Error: {e}", file=sys.stderr)
        sys.exit(1)

if __name__ == "__main__":
    main()
