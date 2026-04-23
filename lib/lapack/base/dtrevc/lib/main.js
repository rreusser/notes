
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dtrevc = require( './dtrevc.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dtrevc, 'ndarray', ndarray );


// EXPORTS //

module.exports = dtrevc;
