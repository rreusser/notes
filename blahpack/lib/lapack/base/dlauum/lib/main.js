

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dlauum = require( './dlauum.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlauum, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlauum;
