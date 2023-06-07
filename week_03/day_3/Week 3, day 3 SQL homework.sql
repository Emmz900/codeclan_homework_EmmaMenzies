/* 1 MVP */


/* Question 1. How many employee records are lacking both a grade and salary? */
SELECT count(id)
FROM employees 
WHERE grade IS NULL
	AND salary IS NULL;


/* Question 2.
 * Produce a table with the two following fields (columns):
 * the department
 * the employees full name (first and last name)
 * Order your resulting table alphabetically by department, and then by last name*/
SELECT 
	department,
	concat(first_name, ' ', last_name) AS full_name
FROM employees
ORDER BY department, last_name;


/* Question 3. Find the details of the top ten highest paid employees who have a last_name beginning with ‘A’.*/
SELECT *
FROM employees 
WHERE last_name ILIKE ('A%')
ORDER BY salary DESC NULLS LAST
LIMIT 10;


/* Question 4. Obtain a count by department of the employees who started work with the corporation in 2003.*/
SELECT 
	department,
	count(id) AS num_started_2003
FROM employees 
WHERE start_date BETWEEN '2003-01-01' AND '2003-12-31'
GROUP BY department;


/* Question 5. Obtain a table showing department, fte_hours 
 * and the number of employees in each department who work each fte_hours pattern. 
 * Order the table alphabetically by department, and then in ascending order of fte_hours.
*/
SELECT 
	department,
	fte_hours,
	count(id)
FROM employees
GROUP BY department, fte_hours 
ORDER BY department, fte_hours
;

/* Question 6. Provide a breakdown of the numbers of employees enrolled, not enrolled, 
 * and with unknown enrollment status in the corporation pension scheme. */
SELECT pension_enrol, count(id)
FROM employees 
GROUP BY pension_enrol;

/* Question 7. Obtain the details for the employee with the highest salary in the ‘Accounting’ department 
 * who is not enrolled in the pension scheme?
*/
SELECT *
FROM employees 
WHERE pension_enrol = FALSE 
ORDER BY salary DESC NULLS LAST 
LIMIT 1;

/* Question 8.
 * Get a table of country, number of employees in that country, and the average salary of employees
 * in that country for any countries in which more than 30 employees are based. 
 * Order the table by average salary descending.
*/
SELECT 
	country,
	count(id) AS n_employees,
	round(avg(salary), 2) AS average_salary
FROM employees
GROUP BY country 
HAVING count(id) > 30
ORDER BY average_salary DESC;



/* Question 9. Return a table containing each employees first_name, last_name, 
 * full-time equivalent hours (fte_hours), salary, 
 * and a new column effective_yearly_salary which should contain fte_hours multiplied by salary. 
 * Return only rows where effective_yearly_salary is more than 30000.
*/
SELECT 
	first_name,
	last_name,
	fte_hours,
	salary,
	fte_hours * salary AS effective_yearly_salary
FROM employees
WHERE (fte_hours * salary) > 30000;


/* Question 10. Find the details of all employees in either Data Team 1 or Data Team 2 */
SELECT e.*, t."name" 
FROM employees AS e
	INNER JOIN teams AS t
	ON e.team_id = t.id 
WHERE t.name = 'Data Team 1' OR t.name = 'Data Team 2';

/* Question 11. Find the first name and last name of all employees who lack a local_tax_code. */
SELECT 
	e.first_name,
	e.last_name
FROM employees AS e
	LEFT JOIN pay_details AS pd 
	ON e.pay_detail_id = pd.id 
WHERE pd.local_tax_code IS NULL;

/* Question 12. The expected_profit of an employee is defined as 
 * (48 * 35 * charge_cost - salary) * fte_hours, 
 * where charge_cost depends upon the team to which the employee belongs. 
 * Get a table showing expected_profit for each employee.
*/
SELECT
	e.id AS employee_id,
	e.first_name,
	e.last_name, 
	(48 * 35 * CAST(t.charge_cost AS int) - e.salary) * e.fte_hours AS expected_profit
FROM employees AS e
	INNER JOIN teams AS t
	ON e.team_id = t.id;



/* Question 13. [Tough]
 * Find the first_name, last_name and salary of the lowest paid employee in Japan
 *  who works the least common full-time equivalent hours across the corporation.”
*/
WITH least_common_fte AS(
	SELECT fte_hours
	FROM employees
	GROUP BY fte_hours 
	ORDER BY count(id)
	LIMIT 1
)
SELECT 
	first_name,
	last_name,
	salary
FROM employees 
WHERE country = 'Japan' 
	AND fte_hours = (SELECT fte_hours FROM least_common_fte)
ORDER BY salary ASC 
LIMIT 1;


/* Question 14. Obtain a table showing any departments in which there are two or more employees 
 * lacking a stored first name. 
 * Order the table in descending order of the number of employees lacking a first name, 
 * and then in alphabetical order by department.
*/
SELECT 
	department
	-- count(first_name IS NULL)
FROM employees
GROUP BY department 
HAVING count(first_name IS NULL) >= 2
ORDER BY count(first_name IS NULL) DESC, department;

/* Question 15. [Bit tougher] Return a table of those employee first_names shared by more than one employee, 
 * together with a count of the number of times each first_name occurs. 
 * Omit employees without a stored first_name from the table. 
 * Order the table descending by count, and then alphabetically by first_name.
*/

-- shared first name | number of times it is shared
SELECT 
	first_name,
	count(first_name)
FROM employees
GROUP BY first_name 
HAVING count(first_name) > 1
ORDER BY count(first_name) DESC, first_name;


/* Question 16. [Tough]
 * Find the proportion of employees in each department who are grade 1.
*/
-- proportion = count(grade 1)/all per department

SELECT 
	department,
	sum(grade) / CAST(count(id) AS REAL) AS proportion
FROM employees AS e
GROUP BY department;


/* 2 Extension
 * Some of these problems may need you to do some online research on SQL statements 
 * we haven’t seen in the lessons up until now… 
 * Don’t worry, we’ll give you pointers, and it’s good practice looking up help and answers online, 
 * data analysts and programmers do this all the time! If you get stuck, it might also help to sketch out 
 * a rough version of the table you want on paper (the column headings and first few rows are usually enough).
 * Note that some of these questions may be best answered using CTEs or window functions: 
 * have a look at the optional lesson included in today’s notes!
*/


/* Question 1. [Tough] Get a list of the id, first_name, last_name, department, salary and 
 * fte_hours of employees in the largest department. 
 * Add two extra columns showing the ratio of each employee’s salary to that department’s average salary, 
 * and each employee’s fte_hours to that department’s average fte_hours.
 * [Extension - really tough! - how could you generalise your query to be able to handle the fact that 
 * two or more departments may be tied in their counts of employees. I
 * n that case, we probably don’t want to arbitrarily return details for employees in just one of these departments].
*/

/* Question 2. Have a look again at your table for MVP question 6. 
 * It will likely contain a blank cell for the row relating to employees with ‘unknown’ pension enrollment status. This is ambiguous: it would be better if this cell contained ‘unknown’ or something similar. Can you find a way to do this, perhaps using a combination of COALESCE() and CAST(), or a CASE statement?
*/



/* Question 3. Find the first name, last name, email address and start date of all the employees who are members of the ‘Equality and Diversity’ committee. Order the member employees by their length of service in the company, longest first.
*/


/* Question 4. [Tough!]
Use a CASE() operator to group employees who are members of committees into salary_class of 'low' (salary < 40000) or 'high' (salary >= 40000). A NULL salary should lead to 'none' in salary_class. Count the number of committee members in each salary_class.*/