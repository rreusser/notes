

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dsyr = require( './dsyr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dsyr, 'ndarray', ndarray );


// EXPORTS //

module.exports = dsyr;
