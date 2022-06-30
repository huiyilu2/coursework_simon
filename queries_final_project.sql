SELECT *
FROM dw_cis467;

# Section 1 Queries for logistic study
-- Q1: which shippers'shipment cycle is longer than average? To which countries are they shipping? --
SELECT ShipperName, AVG(ShipDay) AS Average_ShipDay, ShipCountry
FROM dw_cis467
GROUP BY ShipperName
HAVING AVG(ShipDay) > (SELECT AVG(ShipDay) FROM dw_cis467)
ORDER BY ShipDay DESC;

-- Q2: Which orders were delivered after the required date? How many days were they delayed? -- 
-- Which shippers are responsible for these orders? -- 
-- Which shippers are responsible for most of the delayed orders and have the highest average delayed days? -- 
SELECT OrderID, ShipperName, DelayedDay
FROM dw_cis467
WHERE DelayedDay > 0
GROUP BY OrderID
ORDER BY DelayedDay DESC;

-- design a new index to measure shippers' performance - 'delayindex', the higher the more delays days they had -- 
SELECT ShipperName, AVG(DelayedDay) AS AverageDelay, COUNT(OrderID) AS DelayOrderNumber,
ROUND(AVG(DelayedDay) * COUNT(OrderID),0) AS DelayIndex
FROM dw_cis467
WHERE DelayedDay > 0
GROUP BY ShipperName
ORDER BY AVG(DelayedDay) * COUNT(OrderID) DESC;

# Section 2 Queries for customer study-- 
-- Question3: Who are the TOP25 MVCs (most valued customer) based on purchase frequency and total sales? --
-- what is their demographic information? -- 
-- Most valued customer list (based on total sales and purchase frequency) --
SELECT CustomerID, ROUND(SUM(ProductSales),2) AS Sales_customer, ShipCountry,
-- define frequency as the order placed in 100 days --
COUNT(OrderID)/DATEDIFF(MAX(OrderDate), MIN(OrderDate))*100 AS Frequency
FROM DW_cis467
GROUP BY CustomerID
ORDER BY Sales_customer*Frequency DESC
LIMIT 25;

# Section 3 Queries for customer study --
-- Question4: Which orders has a large/null order-stock ratio? (insufficient availability inventory)--
-- Which suppliers are reponsible for these orders? What products in which categories are in low stock?--
SELECT ProductName, CategoryName, SupplierName,
CASE WHEN OrderToStock_Ratio IS NULL THEN "Sold Out"
WHEN OrderToStock_Ratio > 1 THEN "Not Sufficient Inventory"
WHEN OrderToStock_Ratio < 1 AND OrderToStock_Ratio > 0 THEN "Product In Stock"
ELSE "Not Ordered"
END AS InventoryStatus
FROM dw_cis467
WHERE OrderToStock_Ratio IS NULL OR OrderToStock_Ratio > 1
GROUP BY ProductName
ORDER BY OrderToStock_Ratio DESC;

# Section 4 Queries for employee study --
-- Question5: which sales people performed the best regarding total product sales? What are their titles? 
-- Do sales people with higher title generally perform better? --
-- Do sales people have loyal customers? -- 
SELECT dw_cis467.Salesperson, dw_cis467.Title, dw_cis467.CustomerID AS Loyal_Customer,
ROUND(SUM(ProductSales),2) AS Sales_employee, 
SUM(OrderNumber) AS Order_total
FROM dw_cis467
JOIN (SELECT MAX(TotalSales) AS Max_Sales, Salesperson, CustomerID FROM 
(SELECT CustomerID, Salesperson, ROUND(SUM(ProductSales),2) AS TotalSales
FROM dw_cis467 
GROUP BY Salesperson, CustomerID
ORDER BY SUM(ProductSales) desc) Em
GROUP BY Salesperson
) emstudy ON emstudy.CustomerID = dw_cis467.CustomerID
GROUP BY Salesperson
ORDER BY Sales_employee DESC , Order_total DESC;

-- see the sales under different titles --
SELECT dw_cis467.Title, 
ROUND(SUM(ProductSales),2) AS Sales_employee, 
SUM(OrderNumber) AS Order_total
FROM dw_cis467
JOIN (SELECT MAX(TotalSales) AS Max_Sales, Salesperson, CustomerID FROM 
(SELECT CustomerID, Salesperson, ROUND(SUM(ProductSales),2) AS TotalSales
FROM dw_cis467 
GROUP BY Salesperson, CustomerID
ORDER BY SUM(ProductSales) desc) Em
GROUP BY Salesperson
) emstudy ON emstudy.CustomerID = dw_cis467.CustomerID
GROUP BY Title
ORDER BY Sales_employee DESC , Order_total DESC;

# Section 5 Queries for Supplier study -- 
-- Question6：which suppliers have the most varied products/categories? whose products are most ordered?--
-- How are the suppliers distributed geographically? --
SELECT SupplierName, SupplierCountry, ProductSales, 
SUM(OrderNumber) AS Orders_Total,
COUNT(DISTINCT CategoryName) AS CategoryVariety,
COUNT(DISTINCT ProductName) AS ProductVariety
FROM dw_cis467
GROUP BY SupplierName
ORDER BY Orders_Total DESC, ProductVariety DESC, CategoryVariety DESC;

-- suppliers geographical distribution -- 
SELECT SupplierCountry, ProductSales, 
SUM(OrderNumber) AS Orders_Total,
COUNT(DISTINCT SupplierName) AS SupplierNumber
FROM dw_cis467
GROUP BY SupplierCountry
ORDER BY ProductSales DESC, Orders_Total DESC;

#Section 6 Queries for Product study
-- Question7：which products have the highest sales, order amount and sales per order?
-- and which categories are they in? Are they discontinued?
SELECT ProductName, CategoryName, ProductSales, SUM(OrderNumber) AS Orders_Total, 
ROUND(ProductSales/ SUM(OrderNumber),2) AS SalesPerOrder,
CASE 
WHEN Discontinued = 0 THEN "Not Discontinued"
WHEN Discontinued = 1 THEN "Discontinued"
END AS DiscontinueStatus
FROM dw_cis467
GROUP BY ProductName
ORDER BY ProductSales*Orders_Total, SalesPerOrder DESC;

-- Question8: which products are more frequently discounted? --
-- How much on average are they discounted?-- 
SELECT dw_cis467.ProductName, COUNT(DISTINCT Discount) AS NonDiscounted_Number, 
Quantity, ProductSales, 
pr.Orders_Total, ROUND(AVG(Discount), 2) AS Average_Discount,
COUNT(DISTINCT dw_cis467.Discount)/pr.Orders_total AS Discount_Proportion
FROM dw_cis467 
JOIN (SELECT SUM(OrderNumber) AS Orders_total, ProductName FROM dw_cis467 GROUP BY ProductName) pr 
ON pr.ProductName = dw_cis467.ProductName
WHERE Discount != 0 AND dw_cis467.Discontinued = 0
GROUP BY ProductName
ORDER BY ProductSales DESC



