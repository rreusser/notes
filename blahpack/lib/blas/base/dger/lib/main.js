

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dger = require( './dger.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dger, 'ndarray', ndarray );


// EXPORTS //

module.exports = dger;
