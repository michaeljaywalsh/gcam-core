#ifndef _SECTOR_H_
#define _SECTOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file sector.h
* \ingroup CIAM
* \brief The Sector class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <vector>
#include <xercesc/dom/DOMNode.hpp>
#include <algorithm>
#include <map>

// Forward declarations
class Subsector;
class Summary;
class Emcoef_ind;
class Region;
class ILogger;
class GDP;
class Tabs;

/*! 
* \ingroup CIAM
* \brief This class represents a single good that is produced, transformed, or consumed.

* All production, consumption, and transformation (other than resource extraction) is contained within the Sector class. Each Sector represents a distinct good that can either be supplied or demanded. The demand Sector derived from this class contains a few classes where changes are necessary, although most of the basic mechanisms are unchanged.

* \author Sonny Kim, Steve Smith, Josh Lurz
*/

class Sector
{
private:
    void clear();
protected:
    std::string name; //!< Sector name
    std::string regionName; //!< region name
    std::string unit; //!< unit of final product from Sector
    std::string market; //!< regional market
    int nosubsec; //!< number of subsectors in each Sector
    double tax; //!< Sector tax or subsidy
    bool debugChecking; //!< General toggle to turn on various checks
    std::vector<Subsector*> subsec; //!< subsector objects
    std::vector<double> sectorprice; //!< Sector price in $/service
    std::vector<double> price_norm; //!< Sector price normalized to base year
    std::vector<double> pe_cons; //!< sectoral primary energy consumption
    std::vector<double> input; //!< Sector total energy consumption
    std::vector<double> output; //!< total amount of final output from sector
    std::vector<double> fixedOutput; //!< total amount of fixed output from Sector
    std::vector<double> carbonTaxPaid; //!< total Sector carbon taxes paid
    std::vector<Summary> summary; //!< summary for reporting
    std::map<std::string,int> subSectorNameMap; //!< Map of subSector name to integer position in vector.
    std::vector<bool> capLimitsPresent; //!< Flag if any capacity limits are present 
    std::vector<std::string> simulList; //!< List of all sectors with simuls to this one. 
    std::vector<std::string> dependsList; //!< List of all dependencies of this Sector. 
    bool anyFixedCapacity; //!< flag set to true if any fixed capacity is present in this Sector
    double CO2EmFactor; //! CO2 emissions factor, calculated based on fuel input and share

    virtual void initElementalMembers();
    void sumOutput( const int period ); // private function, sum taken care of automatically
    void sumInput( const int period ); // private function, sum taken care of automatically
    double getFixedShare( const int sectorNum, const int period ) const; // utility function 
    virtual void calcPrice( const int period );
    void production( const int period );
    void adjustForFixedSupply( const double marketDemand, const int period );
    void setServiceDemand( const double demand, const int period );
    void setoutput( const double demand, const int period ); 
    void adjSharesCapLimit( const int period ); 
    void checkShareSum( const int period ) const;
    double getFixedSupply( const int period ) const; 
    bool isCapacityLimitsInSector( const int period ) const;
    virtual void printStyle( std::ostream& outStream ) const;
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const = 0;
    virtual void toOutputXMLDerived( std::ostream& out, Tabs* tabs ) const = 0;
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const = 0;
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr ) = 0;
    virtual bool XMLDerivedClassParseAttr( const xercesc::DOMNode* node ) = 0; // Remove me after fixing input files.
    virtual const std::string& getXMLName() const = 0;
public:
    explicit Sector( std::string regionName );
    virtual ~Sector();
    std::string getName() const;
    virtual void XMLParse( const xercesc::DOMNode* node );
    void completeInit();
    virtual void toInputXML( std::ostream& out, Tabs* tabs ) const;
    virtual void toOutputXML( std::ostream& out, Tabs* tabs ) const;
    virtual void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
    virtual void setMarket();
    void initCalc( const int period );
    virtual void calibrateSector( const int period ); 
    virtual void checkSectorCalData( const int period );
    void setFinalSupply( const int period );
    void setoutput( const double demand, const int period, const GDP* gdp ); 
    void adjustForFixedOutput( const double marketDemand, const int period );
    bool isAllCalibrated( const int period, double calAccuracy, const bool printWarnings ) const;
    void supply( const int period, const GDP* gdp );
    double updateAndGetOutput( const int period );
    double getOutput( const int period ) const;
    void calcFinalSupplyPrice( const GDP* gdp, const int period );
    double getFixedOutput( const int period, bool printValues = false ) const; 
    bool outputsAllFixed( const int period ) const;
    bool inputsAllFixed( const int period, const std::string& goodName ) const;
    double getCalAndFixedInputs( const int period, const std::string& goodName, const bool bothVals = true ) const;
    void scaleCalibratedValues( const int period, const std::string& goodName, const double scaleValue );
    double getPrice( const int period );
    double getCalOutput( const int period ) const;
    virtual void calcShare( const int period, const GDP* gdp );
    void emission( const int period );
    void indemission( const int period, const std::vector<Emcoef_ind>& emcoef_ind );
    double getInput( const int period );
    virtual double getEnergyInput( const int period );
    virtual void csvOutputFile() const;
    virtual void dbOutput() const;
    void subsec_outfile() const;
    double getTotalCarbonTaxPaid( const int period ) const;
    std::map<std::string, double> getfuelcons( const int period ) const;
    double getConsByFuel( const int period, const std::string& key) const;
    void clearfuelcons( const int period );
    std::map<std::string, double> getemission( const int period ) const;
    std::map<std::string, double> getemfuelmap( const int period ) const;
    void updateSummary( const int period );
    void addToDependencyGraph( std::ostream& outStream, const int period ) const;
    void addSimul( const std::string sectorName );
    void setupForSort( const Region* parentRegion );
    std::vector<std::string> getInputDependencies( const Region* parentRegion ) const;
    const std::vector<std::string>& getDependsList() const;
    void printSectorDependencies( ILogger& aLog ) const;

    /*!
    * \brief Binary function used to order Sector* pointers by input dependency. 
    * \author Josh Lurz
    * \detailed This function checks if the right hand side sector has an input dependency
    * on the left hand side Sector. If it does, the right hand side should go after the left
    * hand side, and so is greater than the left hand side, and this operator returns true. 
    */   
    struct DependencyOrdering : public std::binary_function<Sector*, Sector*, bool>
    {
        //! \brief The () operator, which is used for sorting two Sector pointers. 
        bool operator()( const Sector* lhs, const Sector* rhs ) const {
            // First cache a copy of the dependsList
            std::vector<std::string> rhsDependsList = rhs->getDependsList();

            // Check if the right Sector depends on the left Sector.
            return std::binary_search( rhsDependsList.begin(), rhsDependsList.end(), lhs->getName() );
        }
    };
};

#endif // _SECTOR_H_
