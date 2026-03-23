

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zsyr = require( './zsyr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zsyr, 'ndarray', ndarray );


// EXPORTS //

module.exports = zsyr;
