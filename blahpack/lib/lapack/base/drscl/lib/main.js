

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var drscl = require( './drscl.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( drscl, 'ndarray', ndarray );


// EXPORTS //

module.exports = drscl;
