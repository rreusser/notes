
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zunm22 = require( './zunm22.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zunm22, 'ndarray', ndarray );


// EXPORTS //

module.exports = zunm22;
