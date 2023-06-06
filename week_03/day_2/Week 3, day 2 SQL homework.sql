/* Week 3, DAY 2 - Homework */

/* Question 1 */
/*(a). Find the first name, last name and team name of employees 
 * who are members of teams.*/

SELECT 
	e.first_name,
	e.last_name, 
	t.name AS team_name
FROM  employees AS e
	INNER JOIN teams AS t 
	ON e.team_id = t.id;
	
/*(b). Find the first name, last name and team name of employees 
 * who are members of teams and are enrolled in the pension scheme.*/
SELECT 
	e.first_name,
	e.last_name, 
	t.name AS team_name
FROM  employees AS e
	INNER JOIN teams AS t 
	ON e.team_id = t.id
WHERE e.pension_enrol = TRUE;

/*(c). Find the first name, last name and team name of employees 
 * who are members of teams, 
 * where their team has a charge cost greater than 80. */
SELECT 
	e.first_name,
	e.last_name, 
	t.name AS team_name
FROM  employees AS e
	INNER JOIN teams AS t 
	ON e.team_id = t.id
WHERE CAST(t.charge_cost AS int) > 80;

/* Question 2 */
/*(a). Get a table of all employees details, 
 * together with their local_account_no and local_sort_code, 
 * if they have them. */
SELECT 
	e.*,
	pd.local_account_no,
	pd.local_sort_code
FROM 
	employees AS e
	LEFT JOIN pay_details AS pd
	ON e.pay_detail_id = pd.id;

/*(b). Amend your query above to also return the name of the team 
 * that each employee belongs to. */
SELECT 
	e.*,
	pd.local_account_no,
	pd.local_sort_code,
	t."name" AS team_name
FROM 
	(employees AS e
	LEFT JOIN pay_details AS pd
	ON e.pay_detail_id = pd.id)
	INNER JOIN teams AS t 
	ON t.id = e.team_id;

/*Question 3 */
/* a)*/
SELECT 
	e.id,
	t.name AS team_name
FROM  employees AS e
	INNER JOIN teams AS t 
	ON e.team_id = t.id;

/*b + c*/
SELECT 
	t.name AS team_name,
	count(e.id)
FROM  employees AS e
	INNER JOIN teams AS t 
	ON e.team_id = t.id
GROUP BY t.id
ORDER BY count(e.id);

/* Question 4*/
/* (a). Create a table with the team id, team name 
 * and the count of the number of employees in each team.*/
SELECT 
	t.id AS team_id,
	t.name AS team_name,
	count(e.id) AS num_employees
FROM  employees AS e
	INNER JOIN teams AS t 
	ON e.team_id = t.id
GROUP BY t.id
ORDER BY count(e.id);

/*b)*/
SELECT 
	t.id AS team_id,
	t.name AS team_name,
	count(e.id) AS num_employees,
	CAST(t.charge_cost AS int) * count(e.id) AS total_day_charge
FROM  employees AS e
	INNER JOIN teams AS t 
	ON e.team_id = t.id
GROUP BY t.id
HAVING (CAST(t.charge_cost AS int) * count(e.id)) > 5000;

/*Question 5.
How many of the employees serve on one or more committees? */

SELECT 
	count(distinct(e.id))
FROM 
	employees_committees AS ec
	LEFT JOIN employees AS e
	ON ec.employee_id = e.id;

/*Question 6.
How many of the employees do not serve on a committee? */
SELECT 
	count(e.id)
FROM 
	employees_committees AS ec
	RIGHT JOIN employees AS e
	ON ec.employee_id = e.id
WHERE committee_id IS NULL;






